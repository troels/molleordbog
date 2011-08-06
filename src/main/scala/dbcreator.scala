package org.bifrost.molleordbog.dbcreator

import org.bifrost.molleordbog.model._ 
import org.apache.poi.hssf.usermodel._
import org.bifrost.utils.U._
import java.io.{ FileInputStream, File }
import scala.util.matching.Regex
import scala.util.matching.Regex.MatchData
import org.apache.poi.ss.usermodel.Cell.{ CELL_TYPE_STRING, CELL_TYPE_NUMERIC, CELL_TYPE_BLANK }
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
import org.apache.http.protocol.BasicHttpContext
import org.apache.http.params.BasicHttpParams
import org.apache.http.client.params.HttpClientParams
import org.apache.http.impl.client.BasicCookieStore
import org.apache.http.client.protocol.ClientContext

import com.googlecode.objectify.Key
import java.net.URLEncoder
import scala.math.round

trait BaseImporter { 
  Model.isImport = true
}

trait ExcelHelper { 
  def readExcelFile(fileName: String) = new HSSFWorkbook(new FileInputStream(fileName))

  def readCellValue(cell: HSSFCell): String = {
    if(cell == null) return null
    cell getCellType match { 
      case CELL_TYPE_STRING => cell getStringCellValue
      case CELL_TYPE_NUMERIC => (cell getNumericCellValue) toString
      case CELL_TYPE_BLANK => null
      case o => throw new Exception("Unknown type: %s" format (cell getCellType))
    }
  }
}

object OfficeHelpers extends ExcelHelper { 
  case class ParsedWord(source: String, wordNumber: Int, word: String) 
  
  val WordNrSourceRegex = "^(\\d*)[- ](.+)$".r
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

  def readDocFile(fileName: String) = {
    val docFile = new WordExtractor(new FileInputStream(fileName))
    findGroupName(docFile.getParagraphText toList)
  }
  
  
  def findWords(groupName: String, rest: List[String]): Option[DocFileOutput] = { 
    val res = rest findFirst { 
      case lookupwordLineRegex(words) => 
        Some((words split "," toList) map (_.trim) flatMap { 
          case wordRegex(word, number) => Some((word, Integer parseInt number))
          case o => None
        })
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
    
    res flatMap { 
      case (groupTextStart, rest) => 
        val groupText = groupTextStart +: (rest takeWhile { a => !a.isEmpty && !a.startsWith("Tvivlsspørgsmål:") } )
        Some(DocFileOutput(groupName, words, groupText))
    }
  }
}

object ExtractItems extends BaseImporter {
  import OfficeHelpers.{ParsedWord, DocFileOutput }

  val fileName = "/home/troels/src/molleordbog/data/wordlist.xls"
  val docFileDir = new File("/home/troels/src/molleordbog/data/opslagstekster/")
  
  def main(args: Array[String]) { 
    RemoteHandler.withRemoteHandler { 
      collectWordsInDb()
    }
  }
  
  def collectWordsInDb() {
    // val blobstoreService = BlobstoreServiceFactory getBlobstoreService
    
    // (SynonymGroup query) foreach { 
    //   sg => sg pictureKey match { 
    //     case null =>
    //     case key => blobstoreService delete (new BlobKey(key))
    //   }
    // }

    Model.obj.delete(SynonymGroup query)
    Model.obj.delete(Synonym query)

    val items = OfficeHelpers.readItemsXls(fileName)
    val itemsMap: HashMap[Int, List[ParsedWord]] = new HashMap[Int, List[ParsedWord]]()
    
    items foreach {item => 
      itemsMap(item wordNumber) = item :: (itemsMap getOrElse (item wordNumber, Nil))
    }
    
    val files: Iterable[File] = FileUtils.listFiles(
      docFileDir, FileFilterUtils.suffixFileFilter(".doc"), FileFilterUtils.trueFileFilter)

    var sgId: Long = 1
    var synId: Long = 1
    val syns: List[BaseRow[_]] = files flatMap { 
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
                synonymGroup.id = sgId
                sgId += 1
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
                    syn.id = synId
                    synId += 1
                    syn
                  }
                } toList
                
                synonymGroup.text = ((contents groupText) map { 
                  text => "<p>" + escapeHtml(text) + "</p>"
                }) mkString "\n"

                synonymGroup.path = (
                  file.getPath replaceFirst ("^" + (Pattern quote (docFileDir getPath)) + "/*", "") replaceFirst
                  ("\\.doc$", ""))
                synonymGroup.synonyms = (synonyms map { _ getKey }) toList
                
                synonyms foreach {
                  s => s.synonymGroup = synonymGroup getKey
                }
                
                synonymGroup :: synonyms
              } else {
                List[Synonym]()
              }
          } 
      } getOrElse List[Synonym]()
    } toList
    
    Model.obj.putMany(syns :_*)

    val endings = List("", "e", "er", "n", "r", "t", "es", "s", "en", "et", "ens", "ets", 
                       "ers", "ed", "ede", "eders", "eder", "ene")
    val exactWords = List("tolde", "kat", "eg", "skrå", "lig", "hånd", "strå")
    val errorneousWords = List("mus", "hvede", "lus", "ters", "hals", "bos", "ligger", "kors", "aller", "ringe", 
                               "løber", "lås", "plader", "krans", "line", "halv", "hæl", "hus", "hat", "grund", 
                               "sten", "ben", "ret", "led", "skur", "is", "kar", "byg", "let", "kran", "ås",
                               "grene", "råen", "nød", "top", "nøder", "kile", "bille", "skee", "skar", "spes", 
                               "kappe", "lur", "solen", "rine", "hvas", "alle", "bøs", "to sten", "fyr", "skaled", 
                               "koben")

    val synonyms = syns filter { _.isInstanceOf[Synonym] } map { _.asInstanceOf[Synonym] }
    val synonymGroups = syns filter { _.isInstanceOf[SynonymGroup] } map { _.asInstanceOf[SynonymGroup] }

    val synonymWords = (synonyms sortBy { _.word.length } reverse) map { 
      syn => 
        val word = (syn word) toLowerCase
        
        if (exactWords contains word) { 
          (syn, new Regex("\\b" + (Pattern quote word) + "\\b"))
        } else if ((errorneousWords contains word) || (word length) <= 4 ) { 
          (syn, new Regex("\\b" + (Pattern quote word) + "("+ (endings map { Pattern quote _ }  mkString "|") + ")\\b"))
        } else {
          val ending = endings filter { syn.word.endsWith(_) } sortBy { _.length } reverse
          val word = if (ending isEmpty) syn.word else syn.word.substring(0, syn.word.length - ending(0).length)
          (syn, new Regex("\\b" + (Pattern quote word) + "(" + (endings map { Pattern quote _ } mkString "|") + ")\\b"))
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
      
    val groups = synonymGroups map { 
      case sg => { 
        val text = (sg text) toLowerCase

        val intervals = (synonymWords flatMap { 
          case (synonym, regex) => 
            (regex findAllIn text matchData) map { 
              mtch => (synonym, mtch)} toList
        } foldLeft (List[(Synonym, MatchData)]())) { merge(_, _) } sortBy { 
          _._2.start 
        } reverse

        val resText = (intervals foldLeft (sg text)) { 
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

trait FileUploader { 
  val host = "localhost"
  val port = 8080

  def client = new DefaultHttpClient()

  def getUploadUrl(url: String): String =  {
    val get = new HttpGet("http://%s:%d%s" format (host, port, url))
    
    val c = client
    try {
      c execute (get, new BasicResponseHandler)
    } catch {
      case e => {
        println(e)
        getUploadUrl(url)
      }
    } finally {
      c.getConnectionManager.shutdown()
    }
  }
  
  def sendFile(url: String, file: File, contentType: String, otherArgs: Map[String, String]) {
    val post = 
      if (url.startsWith("http://")) {
        new HttpPost(url)
      } else { 
        new HttpPost("http://%s:%d%s" format (host, port, url))
      }
    
    val multipart = new MultipartEntity()

    val fileBody = new FileBody(file, contentType)
    multipart.addPart("blob", fileBody)
    
    otherArgs foreach { 
      case (k, v) => multipart.addPart(k, new StringBody(URLEncoder encode (v, "UTF-8")))
    }
    
    post setEntity multipart
    
    val context = new BasicHttpContext()

    val cookieStore = new BasicCookieStore()
    context.setAttribute(ClientContext.COOKIE_STORE, cookieStore)

    val params = new BasicHttpParams
    HttpClientParams.setRedirecting(params, true)
    post.setParams(params)
    
    val c = client
    try {
      val resp = c execute (post, context)
      val ent = resp getEntity

      if (ent != null) EntityUtils.consume(ent)
    } catch {
      case e => {println(e); throw e; }
    } finally {
      c.getConnectionManager.shutdown
    }
  }
}
  
object PictureExtractor extends FileUploader with BaseImporter { 
  val picDir = new File("/home/troels/src/molleordbog/data/opslag_illustrationer")
  
  def substitutions = List(
    ("timber construction" -> "tinberconstruction"),
    ("securingthewing" -> "securethewing"),
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
  
  def main(args: Array[String]) { 
    RemoteHandler.withRemoteHandler {
      extractPictures()
    }
  }
  
  def extractPictures() { 
    val sgs = (SynonymGroup query) toList
    val sgsMap = sgs map { sg => sg.getKey -> sg } toMap

    val synonyms = ((Synonym query) map { 
      syn => (syn.word.toLowerCase.replaceAll("\\s+", ""), sgsMap(syn.synonymGroup))
    }) ++ (sgs map { sg => (sg canonicalWord).replaceAll("\\s+", "") -> sgsMap(sg.getKey) }) toMap

    val paths = sgs map { sg => findPicPath(sg path) } distinct

    val regex = "^[\\p{javaLowerCase}\\p{javaUpperCase}]+_([\\p{javaLowerCase}\\p{javaUpperCase}]+)_web.*\\.jpg$".r

    (paths distinct) foreach { 
      dir => (dir listFiles) foreach { 
        f => f.getName match { 
          case regex(word) => {
            val lWord = word.toLowerCase
            if (synonyms contains lWord) {
              val url = getUploadUrl("/blobs/uploadUrl")
              sendFile(url, f, "image/jpeg", Map("synonymGroupKey" -> synonyms(lWord).id.toString))
            } else {
              println("Failed to find word: " + word + " " + f.getName)
            }
          }
          case o => println("Failed to handle: " + o)
        }
      }
    }
  }
}

object VisualSearchParser extends ExcelHelper with FileUploader with BaseImporter { 
  val fileName = "/home/troels/src/molleordbog/data/udsnit.xls"
  val imageDir = "/home/troels/src/molleordbog/data/visuel"

  def main(args: Array[String]) {   
    RemoteHandler.withRemoteHandler {
      doIt()
    }
  }

  def doIt() {
    val workbook = readExcelFile(fileName)

    val sheet = workbook getSheetAt 0 
    
    val rows = sheet getPhysicalNumberOfRows
    
    val fields = new HashMap[String, VisualSearchPicture]
    Model.obj.delete(VisualSearchPicture query)
    

    (1 until rows) flatMap { rowNr => safelyNullable(sheet getRow rowNr) } foreach { 
      row => 
        val (pictureName, udsnitsId, xLeft, yUp, xRight, yDown, pointingAtPicture, subject, words) = 
          (readCellValue(row getCell 0), readCellValue(row getCell 1), readCellValue(row getCell 2), 
           readCellValue(row getCell 3), readCellValue(row getCell 4), readCellValue(row getCell 5), 
           readCellValue(row getCell 6), readCellValue(row getCell 7), readCellValue(row getCell 8))

        def parseWords(words: String): List[(Int, Int)] = {
          if (words == null) return List()

          words split "\\s+" map { word => 
            if ((word indexOf "..") != -1) {
              val words = word.split("\\.\\.") map { Integer parseInt _ }
              (words(0) -> words(1))
            } else {
              val num = Integer parseInt word
              (num -> num)
            }
          } toList
        }
      
      def findSynonymGroups(wordList: List[(Int, Int)]): List[Key[SynonymGroup]]  = {
        wordList map { case (lower, upper) => (SynonymGroup query) filter ( 
          "number >= ", lower) filter ("number <= ", upper) fetchKeys 
        } flatMap { iter => iter }
      }
      
      if (pictureName != null) { 
        val vsp = fields getOrElseUpdate (pictureName, new VisualSearchPicture())

        vsp.pictureName = pictureName
        val wordList = parseWords(words)
        
        if (wordList nonEmpty) {
          val groups = findSynonymGroups(wordList)

          if (subject == null) { 
            vsp.words = groups
          } else {
            val subj = Subject(subject, groups toArray)
            
            vsp.subjects = if (vsp.subjects == null) List(subj) toArray else {
              if (!(vsp.subjects exists { s => s.name == subject })) {
                (subj :: (vsp.subjects toList)) toArray
              } else {
                vsp.subjects
              }
            }
          }
        } else if (xLeft != null) { 
          def convert(str: String): Int = round(java.lang.Float parseFloat str)
        
          val x = convert(xLeft); val y = convert(yUp)
          val width = convert(xRight) - x; val height = convert(yDown) - y
            
          val key = new Key[VisualSearchPicture](classOf[VisualSearchPicture], pointingAtPicture)
          val excision = new Excision(x, y, width, height, key)

          vsp.excisions = (if(vsp.excisions == null) List(excision) else (excision :: (vsp.excisions toList))) toArray
        } 
      }
    }

    Model.obj.putMany((fields values) toSeq: _*)
    
    FileUtils.listFiles(new File(imageDir), FileFilterUtils.suffixFileFilter(".jpg"),
                        FileFilterUtils.trueFileFilter) foreach { 
      file => {
        val name = (file getName) replaceAll ("(\\.[jJ][Pp][Gg])+$", ".jpg")
        
        fields get name match {
          case Some(vsp) => 
            val url = getUploadUrl("/blobs/uploadVisual")
            sendFile(url, file, "image/jpeg", Map("key" -> (vsp pictureName)))
          case None => 
            println("Unknown name: " + name)
        }
      }
    }
 }
}


object ReadSourceData extends FileUploader with BaseImporter { 
  val pdfDir = "/home/troels/src/molleordbog/data/statisk_data/spørgelister_samlet"
    
    
    def doIt() { 
      Model.obj.delete(Source query)

      FileUtils.listFiles(new File(pdfDir), FileFilterUtils.suffixFileFilter(".pdf"),
                          FileFilterUtils.trueFileFilter) foreach { 
        file => {
          val name = file.getName 
          val regexp = "^([^_]+)_samlet\\.pdf$" r
          
          name match { 
            case regexp(source) => 
              val s = Source()
              s.name = source
              Model.obj.putOne(s)
            
              val url = getUploadUrl("/blobs/uploadSource")
              sendFile(url, file, "application/pdf", Map("source" -> s.id.toString))
          }
        }
      }
    }

  def main(args: Array[String]) {     
    RemoteHandler.withRemoteHandler { 
      doIt()
    }
  }

}

object AllOfIt { 
  def main(args: Array[String]) { 
    RemoteHandler.withRemoteHandler { 
      ExtractItems.collectWordsInDb()
      PictureExtractor.extractPictures()
      VisualSearchParser.doIt()
      ReadSourceData.doIt()
    }
  }
}
    
