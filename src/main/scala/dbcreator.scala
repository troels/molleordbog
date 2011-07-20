package org.bifrost.molleordbog.dbcreator

import org.bifrost.molleordbog.model._ 
import org.apache.poi.hssf.usermodel._
import org.bifrost.utils.U._
import java.io.{ FileInputStream, File }
import scala.util.matching.Regex
import scala.util.matching.Regex.MatchData
import org.apache.poi.ss.usermodel.Cell.{ CELL_TYPE_STRING, CELL_TYPE_NUMERIC }
import org.apache.poi.hwpf.extractor.WordExtractor
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.FileFilterUtils
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
import org.bifrost.molleordbog.model.{Article, Synonym, Model}
import org.bifrost.molleordbog.model.Implicits._
import com.googlecode.objectify._
import org.bifrost.molleordbog.remoteapi.RemoteHandler
import java.util.regex.Pattern
import com.google.appengine.api.files.FileServiceFactory
import com.google.appengine.api.blobstore.{ BlobstoreServiceFactory, BlobKey }
import java.nio.ByteBuffer

import org.bifrost.counterfeiter.U.escapeHtml
import org.apache.http.{HttpEntity, HttpResponse}
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.{ HttpPost, HttpGet }
import org.apache.http.entity.mime.MultipartEntity
import org.apache.http.impl.client.{DefaultHttpClient, BasicResponseHandler, DefaultRedirectStrategy}
import org.apache.http.util.EntityUtils;
import org.apache.http.entity.mime.content.{FileBody, StringBody}
import java.net.URLEncoder

object OfficeHelpers { 
  def readExcelFile(fileName: String) = new HSSFWorkbook(new FileInputStream(fileName))
  
  case class ParsedWord(source: String, wordNumber: Int, word: String) 
  
  val WordNrSourceRegex = "^(\\d*)[- ](.+)$".r
  
  def readCellValue(cell: HSSFCell): String = {
    if(cell == null) throw new Exception("Cell is null")
    cell getCellType match { 
      case CELL_TYPE_STRING => cell getStringCellValue
      case CELL_TYPE_NUMERIC => (cell getNumericCellValue) toString
      case o => throw new Exception("Unknown type: %s" format (cell getCellType))
    }
  }

  def readItemsXls(fileName: String) =  {
    val workbook = readExcelFile(fileName)
    val sheet = workbook getSheetAt 0 
    
    val rows = sheet getPhysicalNumberOfRows

    0 until rows flatMap { rowNr => safelyNullable(sheet getRow rowNr) } flatMap {
      row => { 
        val (cell0, cell1, cell2) = (row getCell 0, row getCell 1, row getCell 2)

        if (cell0 == null || cell1 == null || cell2 == null) {
          None
        } else {
          val wordId = readCellValue(cell0)
          val wordNr_source = readCellValue(cell1)
          val word = readCellValue(cell2)

          val transform = wordNr_source match  {
            case WordNrSourceRegex(numberString, source)  => {
              val number = if (numberString isEmpty) 0 else Integer parseInt numberString 
              Some((number, source))
            }
            case _ => {
              None
            }
          }
          transform map { case (wordNumber, source) => 
            ParsedWord(source, wordNumber, word)
          }
        }
      }
    }
  }
  
  case class DocFileOutput(groupName: String, words: List[(String, Int)], groupText: List[String]) 

  def readDocFile(fileName: String) = {
    val docFile = new WordExtractor(new FileInputStream(fileName))
    findGroupName(docFile.getParagraphText toList)
  }

  val groupNameRegex = "^\\s*Gruppenavn:\\s*(.*)$".r
  
  def findGroupName(paragraphs: List[String]): Option[DocFileOutput] =  {
    val res = paragraphs map (_ trim) findFirst {
      case groupNameRegex(groupName) => Some(groupName)
      case o => None
    }
    
    res flatMap { case (groupName, rest) => findWords(groupName, rest) }
  }
  
  val lookupwordLineRegex = "^\\s*Opslagsord.*?:(.*)$".r
  val wordRegex = "^(.*)\\s+(\\d+)\\s*".r
  
  def findWords(groupName: String, rest: List[String]): Option[DocFileOutput] = { 
    val res = rest findFirst { 
      case lookupwordLineRegex(words) => {
        (words split "," toList) map (_.trim) flatMap { 
          case wordRegex(word, number) => Some((word, Integer parseInt number))
          case o => None
        }
      }
      case _ => None
    }

    res flatMap { case (words, rest) => findGroupText(groupName, words, rest) }
  }
  
  val groupTextRegex =  "^\\s*Gruppetekst:(.*)$".r
  def findGroupText(groupName: String, words: List[(String, Int)], rest: List[String]): Option[DocFileOutput] = {
    val res = rest findFirst { 
      case groupTextRegex(groupText) => Some(groupText)
      case _ => None
    }
    
    res flatMap { case (groupTextStart, rest) => {
      val groupText = groupTextStart +: (rest takeWhile { a => !a.isEmpty && !a.startsWith("Tvivlsspørgsmål:") } )
      Some(DocFileOutput(groupName, words, groupText))
      }
    }
  }
}

object ExtractItems {
  import OfficeHelpers.{ParsedWord, DocFileOutput }

  case class Word(word: String, number: Int, sources: List[String])
  case class TotalCollection(
    groupName: String, groupText: String, words: List[Word], path: String)
  
  val fileName = "/home/troels/src/molleordbog/data/wordlist.xls"
  val docFileDir = new File("/home/troels/src/molleordbog/data/opslagstekster/")
  
  def main(args: Array[String]) { 
    RemoteHandler.withRemoteHandler { 
      collectWordsInDb()
      PictureExtractor.extractPictures()
    }
  }
  
  def collectWordsInDb() {
    val blobstoreService = BlobstoreServiceFactory getBlobstoreService

    (Synonym query) foreach { 
      syn => syn pictureKey match { 
        case null =>
        case key => blobstoreService delete (new BlobKey(key))
      }
    }

    Model.obj.delete(Synonym query)
    Model.obj.delete(Article query)

    val items = OfficeHelpers.readItemsXls(fileName)
    val itemsMap: HashMap[Int, List[ParsedWord]] = new HashMap[Int, List[ParsedWord]]()
    
    items foreach {item => 
      itemsMap(item wordNumber) = item :: (itemsMap getOrElse (item wordNumber, Nil))
    }
    
    val files: Iterable[File] = FileUtils.listFiles(
      docFileDir, FileFilterUtils.suffixFileFilter(".doc"), FileFilterUtils.trueFileFilter)

    val collections = files flatMap { 
      file => (OfficeHelpers readDocFile file.getPath) map { 
        contents => 
          val words = (contents words) flatMap { 
            case (word, number) =>
              if (itemsMap contains number) {
                val _words = itemsMap(number)
                val sources = _words map { _ source } distinct

                Some(Word(word, number, sources))
              } else { 
                List()
              }
          }
        
        TotalCollection(contents groupName, (contents groupText) mkString "\n", words, 
                        file.getPath replaceFirst ("^" + Pattern.quote(docFileDir.getPath) + "/*", "")
                        replaceFirst ("\\.doc$", ""))
      }
    }
    
    collections foreach {
      addToDb(_)
    }
    
    val articles = (Article query) toList
    val endings = List("", "e", "er", "n", "r", "t", "es", "s", "en", "et", "ens", "ets", "ers", "ed", "ede", "eders", "eder", "ene")
    val exactWords = List("tolde", "kat", "eg", "skrå", "lig", "hånd", "strå")
    val errorneousWords = List("mus", "hvede", "lus", "ters", "hals", "bos", "ligger", "løber", "lås", "plader", "krans", "line", "halv", "hæl", "hus", "hat")

    val synonyms = (Synonym query) map { 
      syn => 
        val word = (syn word) toLowerCase
        
        if (exactWords contains word) { 
          (syn, new Regex("\\b" + (Pattern quote word) + "\\b"))
        } else if (errorneousWords contains word) { 
          (syn, new Regex("\\b" + (Pattern quote word) + "(" + (endings map { Pattern quote _ }  mkString "|") + ")\\b"))
        } else {
          val ending = endings filter { syn.word.endsWith(_) } sortBy { _.length } reverse
          val word = if (ending isEmpty) syn.word else syn.word.substring(0, syn.word.length - ending(0).length)
          (syn, new Regex("\\b" + (Pattern quote word.toLowerCase) +  "[\\p{javaLowerCase}\\p{javaUpperCase}]*\\b"))
        }
    } 
    
    def overlaps(m0: MatchData, m1: MatchData): Boolean = 
      m0.start <= m1.start && m1.start < m0.end || m1.start <= m0.start && m0.start < m1.end

    def merge(lst: List[(Synonym, MatchData)], m: (Synonym, MatchData)): List[(Synonym, MatchData)] = {
      lst find { case (s, md) => overlaps(md, m._2) } match {
        case None => m :: lst 
        case Some((s, md)) => 
          if (m._2.end - m._2.start <= md.end - md.start) {
            lst
          } else {
            merge(lst filterNot { 
              case (_, md_) => md.start == md.start && md.end == md_.end
            }, m)
          }
      }
    }
          

    articles foreach { 
      case article => { 
        val text = (article text) toLowerCase

        val intervals = (synonyms flatMap { 
          case (synonym, regex) => 
            (regex findAllIn text matchData) map { 
              mtch => (synonym, mtch)} toList
        } foldLeft (List[(Synonym, MatchData)]())) { merge(_, _) } sortBy { 
          _._2.start 
        } reverse

        val resText =(intervals foldLeft (article text)) { 
          (text, synmd) => synmd match {
            case (syn, md) => 
              if (syn.article == article.getKey) { 
                text.substring(0, md.start) + "<span class=\"ownlink\">" + text.substring(md.start, md.end) + 
                "</span>" + text.substring(md.end)
              } else {
                text.substring(0, md.start) + "<a class=\"interlink\" href=\"/ordbog/opslag/?ord=" + 
                (URLEncoder encode (syn.word, "UTF-8")) + "\">" +
                text.substring(md.start, md.end) + "</a>" + text.substring(md.end)
              }
          }
        }

        article.text = resText
      }
    }
    Model.obj.putMany(articles :_*)
  }

  def addToDb(collection: TotalCollection) { 
    val words = (collection words) map { 
      word =>
        val synonym = new Synonym 
        synonym.word = (word word)
        synonym.number = (word number)
        synonym.sources = (word sources)
        synonym
    }
    Model.obj.put(words :_*)

    val ids = words map { word => new Key(classOf[Synonym], word.id longValue) } 
    val text = escapeHtml(collection groupText) split "[\n\r]+" map { 
      "<p>" + _ + "</p>"} mkString "\n"

    val article = new Article()
    article.groupName = collection.groupName
    article.mainSynonym = words(0).word
    article.text = text
    article.words = ids
    article.path = collection.path
    val k = Model.obj putOne article
    
    words foreach { 
      w => w.article = k
    }

    Model.obj.put(words :_*)
  }
}

object PictureExtractor { 
  val picDir = new File("/home/troels/src/molleordbog/data/opslag_illustrationer")
  
  def substitutions = List(
    ("timber construction" -> "tinberconstruction"),
    ("securingthewing" -> "secure the wing"),
    ("fourpiecewing" -> "fourpiecewings"),
    ("sailmaterials" -> "sail materials"),
    ("sailproofing" -> "sail proofing"),
    ("sailropes" -> "sail ropes"),
    ("wooddensails_DM" -> "woodden sails"),
    ("settingofthesails" -> "setting of the sails"),
    ("mountingofsails" -> "mounting of sails"),
    ("selfreefing_DM" -> "selfreefing"),
    ("møllens indretning/floorsections", "møllensindretning/floor sections"),
    ("windingrings" -> "winding rings"),
    ("windingsystems" -> "winding systems"),
    ("strongwind" -> "strong wind"))

  def findPicPath(pathPart: String): File = {
    var f = new File(picDir, pathPart) 
    if (f exists) return f

    f = new File(picDir, pathPart replaceAll("\\s+", ""))
    if (f exists) return f
    
    f = (substitutions findFirst { 
      sub => 
        val fn = new File(picDir, pathPart replace (sub._1, sub._2)) 
        if (fn exists) Some(fn) else None
    } get) _1

    f
  }
  
  val host = "localhost"
  val port = 8080
  
  def extractPictures() { 
    val blobstoreService = BlobstoreServiceFactory getBlobstoreService
      
    Article.query foreach { 
      article => 
        
      val dir = findPicPath(article path)

      var synonyms = Synonym get (article words) values
      
      val regex = "^[\\p{javaLowerCase}\\p{javaUpperCase}]+_([\\p{javaLowerCase}\\p{javaUpperCase}]+)_web.*\\.jpg$".r
      (dir listFiles) foreach { 
        f => f.getName match { 
          case regex(word) => 
            synonyms filter { syn => syn.word.toLowerCase.replaceAll(" ", "") == word.toLowerCase } match {
              case synonym :: lst => 
                val url = getUploadUrl(host, port, "/blobs/uploadUrl")
                sendFile(host, port, url, f, "image/jpeg", Map("synonymKey" -> synonym.id.toString))
              case _ => 
            }
          case _ =>
        }
      }
    }
  }

  lazy val client = new DefaultHttpClient()

  def getUploadUrl(host: String, port: Int, url: String): String =  {
    val get = new HttpGet("http://%s:%d%s" format (host, port, url))
    
    client.execute(get, new BasicResponseHandler)
  }
  
  def sendFile(host: String, port: Int, url: String, file: File, 
               contentType: String, otherArgs: Map[String, String]) {
    val post = new HttpPost("http://%s:%d%s" format (host, port, url))
    
    val multipart = new MultipartEntity()

    val fileBody = new FileBody(file)
    multipart.addPart("blob", fileBody)
    
    otherArgs foreach { 
      case (k, v) => multipart.addPart(k, new StringBody(v))
    }
    
    post setEntity multipart
    
    try { 
      client execute (post, new BasicResponseHandler)
    } catch {
      case e => 
    }
  }
}
