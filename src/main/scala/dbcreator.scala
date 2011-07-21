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
import org.bifrost.molleordbog.model.{Synonym, SynonymGroup, Model}
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

    (SynonymGroup query) foreach { 
      sg => sg pictureKey match { 
        case null =>
        case key => blobstoreService delete (new BlobKey(key))
      }
    }

    Model.obj.delete(SynonymGroup query)
    Model.obj.delete(Synonym query)

    val items = OfficeHelpers.readItemsXls(fileName)
    val itemsMap: HashMap[Int, List[ParsedWord]] = new HashMap[Int, List[ParsedWord]]()
    
    items foreach {item => 
      itemsMap(item wordNumber) = item :: (itemsMap getOrElse (item wordNumber, Nil))
    }
    
    val files: Iterable[File] = FileUtils.listFiles(
      docFileDir, FileFilterUtils.suffixFileFilter(".doc"), FileFilterUtils.trueFileFilter)

    val syns: List[Synonym] = files flatMap { 
      file => 
        (OfficeHelpers readDocFile file.getPath) map { 
          contents => (contents words) flatMap { 
            case (word, number) =>
              if (itemsMap contains number) {
                val _words = itemsMap(number)
                val sources = _words map { _ source } distinct
                
                val synonymGroup = SynonymGroup()
                synonymGroup.canonicalWord = word
                synonymGroup.number = number
                
                val wordMap: HashMap[String, List[String]] = new HashMap
                
                _words foreach { 
                  pw => {
                    val word = pw.word.toLowerCase.replaceAll("\\]|\\[", "").trim
                    wordMap(word) = pw.source :: (wordMap getOrElse (word, List()))
                  }
                }

                val synonyms = wordMap map { 
                  case (k, v) => {
                    val syn = new Synonym
                    syn.word = k
                    syn.sources = v
                    syn
                  }
                } toList
                
                Model.obj.putMany(synonyms :_*)
                
                synonymGroup.text = ((contents groupText) map { 
                  text => "<p>" + escapeHtml(text) + "</p>"
                }) mkString "\n"

                synonymGroup.path = (
                  file.getPath replaceFirst ("^" + (Pattern quote (docFileDir getPath)) + "/*", "") replaceFirst
                  ("\\.doc$", ""))
                synonymGroup.synonyms = (synonyms map { _ getKey }) toList
                
                synonymGroup.save()
                synonyms foreach {
                  s => s.synonymGroup = synonymGroup getKey
                }
                
                synonyms
              } else {
                List[Synonym]()
              }
          } 
      } getOrElse List[Synonym]()
    } toList
    
    Model.obj.putMany(syns : _*)
    
    val endings = List("", "e", "er", "n", "r", "t", "es", "s", "en", "et", "ens", "ets", 
                       "ers", "ed", "ede", "eders", "eder", "ene")
    val exactWords = List("tolde", "kat", "eg", "skrå", "lig", "hånd", "strå")
    val errorneousWords = List("mus", "hvede", "lus", "ters", "hals", "bos", "ligger", "kors", "aller", "ringe", 
                               "løber", "lås", "plader", "krans", "line", "halv", "hæl", "hus", "hat", "grund", 
                               "sten", "ben", "ret", "led", "skur", "is", "kar", "byg", "let", "kran", "ås",
                               "grene", "råen", "nød", "top", "nøder", "kile", "bille", "skee", "skar", "spes", 
                               "lur", "solen", "rine")

    val synonyms = ((Synonym query) toList) sortBy { _.word.length } map { 
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
          

    val groups = (SynonymGroup.query) map { 
      case sg => { 
        val text = (sg text) toLowerCase

        val intervals = (synonyms flatMap { 
          case (synonym, regex) => 
            (regex findAllIn text matchData) map { 
              mtch => (synonym, mtch)} toList
        } foldLeft (List[(Synonym, MatchData)]())) { merge(_, _) } sortBy { 
          _._2.start 
        } reverse

        val resText =(intervals foldLeft (sg text)) { 
          (text, synmd) => synmd match {
            case (syn, md) => 
              if (syn.synonymGroup == sg.getKey) { 
                text.substring(0, md.start) + "<span class=\"ownlink\">" + text.substring(md.start, md.end) + 
                "</span>" + text.substring(md.end)
              } else {
                text.substring(0, md.start) + "<a class=\"interlink\" href=\"/ordbog/opslag/?ord=" + 
                (URLEncoder encode (syn.word, "UTF-8")) + "\">" +
                text.substring(md.start, md.end) + "</a>" + text.substring(md.end)
              }
          }
        }

        sg.text = resText
        sg
      }
    } toList

    Model.obj.putMany(groups: _*)
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
    
    val synonyms = (Synonym query) map { syn => (syn.word.toLowerCase.replaceAll(" ", ""), syn.getSynonymGroup) } toMap
    
    val paths = (SynonymGroup query)  map { sg => findPicPath(sg path) }
    val sgWord = (SynonymGroup query) map { sg => (sg.canonicalWord -> sg) } toMap
    val regex = "^[\\p{javaLowerCase}\\p{javaUpperCase}]+_([\\p{javaLowerCase}\\p{javaUpperCase}]+)_web.*\\.jpg$".r

    paths foreach { 
      dir => (dir listFiles) foreach { 
        f => f.getName match { 
          case regex(word) => {
            val lWord = word.toLowerCase
            if (synonyms contains lWord) {
              val url = getUploadUrl(host, port, "/blobs/uploadUrl")
              sendFile(host, port, url, f, "image/jpeg", Map("synonymGroupKey" -> synonyms(lWord).id.toString))
            } else if (sgWord contains lWord) {
              val url = getUploadUrl(host, port, "/blobs/uploadUrl")
              sendFile(host, port, url, f, "image/jpeg", Map("synonymGroupKey" -> sgWord(lWord).id.toString))
            } else {
              //println("Failed to find word: " + word + " " + f.getName)
            }
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
