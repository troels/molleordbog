package org.bifrost.molleordbog.dbcreator

import org.bifrost.molleordbog.model._ 
import org.apache.poi.hssf.usermodel._
import org.bifrost.utils.U._
import java.io.{ FileInputStream, File }
import scala.util.matching.Regex
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


import org.apache.http.{HttpEntity, HttpResponse}
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.{ HttpPost, HttpGet }
import org.apache.http.entity.mime.MultipartEntity
import org.apache.http.impl.client.{DefaultHttpClient, BasicResponseHandler, DefaultRedirectStrategy}
import org.apache.http.util.EntityUtils;
import org.apache.http.entity.mime.content.{FileBody, StringBody}

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
          println("Error with row: %d" format (row getRowNum))
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
              println("Problem with row: %d, contains: %s" format (row getRowNum, wordNr_source))
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
      val groupText = groupTextStart +: (rest takeWhile (!_.isEmpty))
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
    RemoteHandler.setUp()
    collectWordsInDb()
    RemoteHandler.tearDown()
  }
  
  def collectWordsInDb() {
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
                val words = itemsMap(number)
                val sources = words map (_ source)
                Some(Word(word, number, sources))
              } else { 
                None
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
    
    val ids = words map { word => new Key(classOf[Synonym], word.number longValue) } 
    val article = new Article()
    article.groupName = collection.groupName
    article.mainSynonym = words(0).word
    article.text = collection.groupText
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

  def main(args: Array[String]) { 
    RemoteHandler.withRemoteHandler {
      val blobstoreService = BlobstoreServiceFactory getBlobstoreService
      
      Article.query foreach { 
        article => 
          article pictures match { 
            case null => 
            case pics => blobstoreService delete (pics map { new BlobKey(_) } : _*)
          }
          article.pictures = List()
          article.save()
        
          val dir = findPicPath(article path)
          
          (dir listFiles) foreach { 
            file => 
              if (file.getPath endsWith ".jpg") {
                val url = getUploadUrl(host, port, "/blobs/uploadUrl/")

                sendFile(host, port, url, file, "image/jpeg", Map("articleKey" -> article.id.toString))
              }
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
