package org.bifrost.molleordbog.views

import org.bifrost.utils.mapping._
import org.bifrost.utils.http._
import org.bifrost.molleordbog.model._
import org.bifrost.utils.U._

import org.bifrost.utils.templates.TemplateResponse
import java.text.Collator
import java.util.Locale
import com.google.appengine.api.images.ImagesServiceFactory

import scala.collection.JavaConversions._
import com.google.appengine.api.blobstore.{ BlobstoreServiceFactory, BlobKey, BlobInfoFactory }

import java.net.{URLEncoder, URLDecoder}

object Utils { 
  lazy val collator = Collator.getInstance(new Locale("da", "dk"))
}

object MolleOrdbogMappings extends BaseMapping {
  lazy val mappings: Mapping = 
    (FrontpageMapping() ==> Views.frontpage) |
    ("soeg" ==> Views.search) |
    ("visuel" / (
      (FrontpageMapping() ==> Views.visualStart) |
      ("browse" / MapperWord("picture") ==> Views.millPart) |
      ("moller" / MapperList("milltype", "vandmolle", "stubmolle", "hollander") / 
       ((FrontpageMapping() ==> Views.millChoice) | 
        (MapperList("inout", "inde", "ude") ==> Views.millPart))))) |
    ("ordbog" / "autocomplete" ==> Views.autocomplete) | 
    ("ordbog" / "opslag" ==>  Views.lookup) |
    ("ordbog" / "nummer" ==>  Views.lookupSynonymgroup) |
    ("blobs" / (
      ("cleanemout" ==> Views.removeBlobs) |
      ("getBlob" ==> Views.getBlob) |
      ("uploadVisual" ==> Views.uploadVisualUrl) |
      ("uploadVisualRedirect" ==> Views.uploadVisualRedirect) |
      ("uploadSourcePdf" ==> Views.uploadSourcePdfUrl) | 
      ("sourcePdfRedirect" ==> Views.uploadSourcePdfRedirect) | 
      ("uploadSourcePicture" ==> Views.uploadSourcePictureUrl) | 
      ("sourcePictureRedirect" ==> Views.uploadSourcePictureRedirect) | 
      ("uploadPagePicture" ==> Views.uploadPagePictureUrl) | 
      ("pagePictureRedirect" ==> Views.uploadPagePictureRedirect) | 
      ("uploadUrl" ==> Views.uploadUrl) |
      ("uploadRedirect" ==> Views.uploadRedirect) |
      ("uploadDone" ==> Views.uploadDone))) |
   ("kilder" / ( 
     ("oversigt" ==> Views.allSources) |
     ("viskilde" / MapperWord("id")) ==> Views.showSource)) |
   ("cms" / CMSMapping)
}

object CMSMapping extends Mapping {
  override def apply(req: Request, uriParts: List[String]) = {
    val uri = "/" + (uriParts mkString "/") toLowerCase
    
    Page getOption uri map { 
      page => (req putRequestAttribute ("page", page), Views.cms)
    }
  }
}


object Views { 
  type View = Request => Response

  def blobstoreService = BlobstoreServiceFactory getBlobstoreService
  def withArg(arg: String)(f: (Request, String) => Response)(req: Request): Response =
    req getArg arg map { 
      argVal => f(req, argVal)
    } getOrElse {
      RequestErrorResponse(Some("Missing argument: %s" format arg))
    }
  
  def withArg(arg0: String, arg1: String)(f: (Request, String, String) => Response)(req: Request): Response =
    req getArg arg0 map { 
      argVal0 => withArg(arg1)( (req, argVal1) => f(req, argVal0, argVal1) )(req) 
    } getOrElse {
      RequestErrorResponse(Some("Missing argument: %s" format arg0))
    }
  
      
  def getBlob: View = withArg("blob") { 
    (req, protoBlobKey) => 
      val blobKey  = new BlobKey(protoBlobKey)
      val blobInfo = (new BlobInfoFactory() loadBlobInfo blobKey)
      val mimeType = (blobInfo getContentType)
      val fileName = (blobInfo getFilename)

      BlobResponse((req originalRequest) get, mimeType, blobKey, Some(fileName))
  }

  val millNames = Map(
    "hollænder" -> "Hollændermølle",
    "stubmølle" -> "Stubmølle",
    "vandmølle" -> "Vandmølle",
    "vindmøller" -> "Vindmølle",
    "vind_vandmøller" -> "Vind/vandmølle")
  
  val reverseMillNames = millNames map { case (k, v) => (v -> k) } toMap

  def lookup: View = withArg("ord") { 
    (req, word) => 
      val (realWord: String, millType: String) = 
        if (word contains " - ") { 
          val parts = word split (" - ", 2) map { _.trim.toLowerCase }
          (parts(0) -> parts(1))
        } else {
          ((word trim) -> "")
        }
    
      val synonyms = Synonym findSynonyms realWord 
    
      if (synonyms isEmpty) { 
        RedirectResponse("/soeg/?ord=" + (URLEncoder encode (word, "UTF-8")))
      } else {
        val syn = 
          if (millType.contains("vand") && millType.contains("vind") && (synonyms.filter { _.millType == "vind_vandmøller" } nonEmpty)) { 
            synonyms filter { _.millType == "vind_vandmøller" } head
          } else if (millType.contains("vand") && (synonyms filter { _.millType == "vandmølle" } nonEmpty)) {
            synonyms filter { _.millType == "vandmølle" } head
          } else if (millType.contains("vind") && (synonyms filter { _.millType == "vindmøller" } nonEmpty)) {
            synonyms filter { _.millType == "vindmøller" } head
          } else if (millType.contains("hollænder") && (synonyms filter { _.millType == "hollænder" } nonEmpty)) {
            synonyms filter { _.millType == "hollænder" } head
          } else if (millType.contains("stubmølle") && (synonyms filter { _.millType == "stubmølle" } nonEmpty)) {
            synonyms filter { _.millType == "stubmølle" } head
          } else {
            synonyms head
          }
        TemplateResponse(
          "main.article", 
          "word" -> realWord,
          "synonym" -> syn, 
          "article" -> (syn getSynonymGroup))
      }

  }
  
  def lookupSynonymgroup: View = withArg("nummer") { 
    (req, number) => 
      val sg = SynonymGroup get (java.lang.Long parseLong number)
    
    TemplateResponse(
      "main.article", 
      "article" -> sg
    )
  }

  def autocomplete: View = withArg("term") {
    (req, word) => 
      val synonyms = (Synonym findWithPrefix word toList) map { syn => ("%s - %s" format (syn.word, millNames getOrElse (syn.millType, syn.millType))) } 
      
      JSONResponse(synonyms take 10)
  }
  
  def uploadVisualUrl: View = {
    _ => TextResponse(blobstoreService createUploadUrl "/blobs/uploadVisualRedirect/")
  }
    
  def uploadUrl: View = {
    _ => TextResponse(blobstoreService createUploadUrl "/blobs/uploadRedirect/")
  }

  def uploadSourcePdfUrl: View = {
    _ => TextResponse(blobstoreService createUploadUrl "/blobs/sourcePdfRedirect/")
  }

  def uploadSourcePictureUrl: View = {
    _ => TextResponse(blobstoreService createUploadUrl "/blobs/sourcePictureRedirect/")
  }

  def uploadPagePictureUrl: View = {
    _ => TextResponse(blobstoreService createUploadUrl "/blobs/pagePictureRedirect/")
  }

  val uploadVisualRedirect: View = 
    genericUploadRedirect("key", {
      k => VisualSearchPicture get k
    }, Some({
      (vsp: VisualSearchPicture, blobKey: BlobKey) => {
        val img = try {
          ImagesServiceFactory makeImageFromBlob blobKey
        } catch {
          case e=> {
            println("Failed with blobkey; " + blobKey)
            throw e
        }
      }
        
      val newImg = (ImagesServiceFactory getImagesService) applyTransform (
        ImagesServiceFactory makeCrop (0, 0, 1, 1), img)
      
      vsp.width = (newImg getWidth)
      vsp.height = (newImg getHeight)

      val url = (ImagesServiceFactory getImagesService) getServingUrl blobKey
      vsp.pictureUrl = url
        
      }
    }))
  
  type TakesUpload = { 
    def save(): BaseRow[_]
  }
    
  def genericUploadRedirect[T <: TakesUpload](argName: String, getObj: String => T, 
                                              beforeSave: Option[(T, BlobKey) => Unit] = None,
                                              keyAttribute: String = "pictureKey") : View = withArg(argName) { 
    (req, protoKey) =>  {
      val key = URLDecoder decode (protoKey, "UTF-8")
      val blobKey = (blobstoreService getUploadedBlobs (req.originalRequest get)) get "blob"
      
      val obj = getObj(key)
      
      val getter = obj.getClass.getMethod(keyAttribute)
      val setter = obj.getClass.getMethod("%s_$eq" format keyAttribute, classOf[String])
      
      if (getter.invoke(obj) != null) {
        blobstoreService delete new BlobKey(getter.invoke(obj).asInstanceOf[String])
      }
      
      setter.invoke(obj, blobKey getKeyString)
      
      beforeSave map { bs => bs(obj, blobKey) } 

      obj.save()
      RedirectResponse("/blobs/uploadDone/")
    }
  }

  val uploadRedirect: View = genericUploadRedirect("synonymGroupKey", { 
    k => SynonymGroup get (java.lang.Long parseLong k) } , Some({
    (obj: SynonymGroup, blobKey: BlobKey) => 
      val pictureUrl = (ImagesServiceFactory getImagesService) getServingUrl blobKey
      obj.pictureUrl = pictureUrl
    }))
  
  val uploadSourcePdfRedirect: View = genericUploadRedirect("source", {
    k => Source get k}, keyAttribute = "pdfKey")
  
  val uploadSourcePictureRedirect: View = genericUploadRedirect("source", {
    k => Source get k
  }, 
    Some({(obj: Source, blobKey: BlobKey) => 
      val pictureUrl = (ImagesServiceFactory getImagesService) getServingUrl blobKey
      obj.pictureUrl = pictureUrl
    }), "pictureKey")

  val uploadPagePictureRedirect: View = genericUploadRedirect("page", {
    k => Page get k 
  }, Some({(obj: Page, blobKey: BlobKey) => 
      val pictureUrl = (ImagesServiceFactory getImagesService) getServingUrl blobKey
      obj.pictureUrl = pictureUrl
  }), "pictureKey")
                                                              
  def uploadDone: View = { _ => TextResponse("Upload complete") }
  
  def visualStart: View = { 
    req => TemplateResponse("main.visualsearch")
  }

  def millChoice: View = {
    req => TemplateResponse("main.milltype", "milltype" -> (req getRequestAttribute "milltype" get))
  }

  val millConverter = Map("hollander" -> "holl", 
                          "stubmolle" -> "stubmølle",
                          "vandmolle" -> "vandmølle")
                          
  def getVsp(req: Request): VisualSearchPicture = {
    VisualSearchPicture get (
      req getRequestAttribute "picture" map { 
        pic => URLDecoder decode (pic.asInstanceOf[String], "UTF-8")
      } getOrElse {
        val milltype = req getRequestAttribute "milltype" map { str => millConverter(str.asInstanceOf[String]) } get
        val inout = req getRequestAttribute "inout" get

        "%s_%s.jpg" format (milltype, if (milltype == "vandmølle" && inout == "ude") "skema" else inout)
      }
    )
  }
    
  
  case class StudiedSubject(name: String, subjects: List[SynonymGroup])

  def millPart: View = {
    req => {
      val vsp = getVsp(req)
      val wordsAsync = if (vsp.words != null) Model.obj.async get(vsp words) else null
      val subjects = vsp subjects

      val words = 
        if (wordsAsync != null) 
          List(StudiedSubject("Illustration", wordsAsync.get.values.toList)) 
        else 
          List()
      
      val studiedSubjects = if (subjects != null) { 
        subjects map { case Subject(name, keys) => (name, Model.obj.async get (keys toSeq)) } map { 
          case (name, vsps) => StudiedSubject(name, vsps.get.values.toList)
        } toList
      } else List()
      
      
      TemplateResponse(if (req isAjax) "main.millbrowser_ajax" else  "main.millbrowser", 
                       "vsp" -> vsp, "subjects" -> (words ++ studiedSubjects))
    }
  }
  
  def search: View = { 
    req => TemplateResponse("main.search", "seed" -> (req getArg "ord" getOrElse ""))
  }

  def allSources: View = { 
    req => TemplateResponse("main.sources", "sources" -> (Source.query toList))
  }


  def showSource: View = {
    req => 
      val sourceId = req getRequestAttribute "id" get
      val source = Source get sourceId.asInstanceOf[String]
    
      TemplateResponse("main.show_source", "source" -> source)
  }

  def removeBlobs: View = { 
    req => {
      val bs = (BlobstoreServiceFactory getBlobstoreService) 

      (new BlobInfoFactory() queryBlobInfos) foreach { 
        bi => bs delete (bi getBlobKey)
      }
        
      TextResponse("All is gone")
    }
  }

  def cms: View = {
    req => {
      val page = (req getRequestAttribute "page").get.asInstanceOf[Page]
      TemplateResponse("main.cms", "page" -> page)
    }
  }

  def frontpage: View = {
    req => TemplateResponse("main.frontpage")
  }
}
