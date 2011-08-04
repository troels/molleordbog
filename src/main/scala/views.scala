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
import com.google.appengine.api.blobstore.BlobstoreServiceFactory
import com.google.appengine.api.blobstore.BlobKey

import java.net.{URLEncoder, URLDecoder}

object Utils { 
  lazy val collator = Collator.getInstance(new Locale("da", "dk"))
}

object MolleOrdbogMappings extends BaseMapping {
  lazy val mappings: Mapping = 
    (FrontpageMapping() ==> Views.search) |
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
      ("uploadVisual" ==> Views.uploadVisualUrl) |
      ("uploadVisualRedirect" ==> Views.uploadVisualRedirect) |
      ("getBlob" ==> Views.getBlob) |
      ("uploadUrl" ==> Views.uploadUrl) |
      ("uploadRedirect" ==> Views.uploadRedirect) |
      ("uploadDone" ==> Views.uploadDone)))
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
    (req, blobKey) => 
      BlobResponse((req originalRequest) get, "image/jpeg", new BlobKey(blobKey))
  }

  def lookup: View = withArg("ord") { 
    (req, word) => 
      Synonym findSynonym word match {
        case None => new RedirectResponse("/?ord=" + (URLEncoder encode (word, "UTF-8")))
        case Some(syn) => TemplateResponse(
          "main.article", 
          "synonym" -> syn, 
          "article" -> (syn getSynonymGroup))
      }
  }
  
  def lookupSynonymgroup: View = withArg("nummer") { 
    (req, number) => 
      val sg = SynonymGroup get (java.lang.Long parseLong number)
    
    TemplateResponse(
      "main.article", 
      "synonym" -> (Synonym get sg.synonyms.get(0)),
      "article" -> sg
    )
  }

  def autocomplete: View = withArg("term") {
    (req, word) => 
      val synonyms = (Synonym findWithPrefix word toList) map (_ word) take 10
      
      JSONResponse(synonyms)
  }
  
  def uploadVisualUrl: View = {
    _ => TextResponse(blobstoreService createUploadUrl "/blobs/uploadVisualRedirect/")
  }
    
  def uploadUrl: View = {
    _ => TextResponse(blobstoreService createUploadUrl "/blobs/uploadRedirect/")
  }

  def uploadVisualRedirect: View = withArg("key") { 
    (req, k) => {
      val key = URLDecoder decode (k, "UTF-8")
      val blobKey = (blobstoreService getUploadedBlobs (req.originalRequest get)) get "blob"
      val url = (ImagesServiceFactory getImagesService) getServingUrl blobKey
      val vsp = VisualSearchPicture get key
      
      if (vsp.pictureKey != null) {
        blobstoreService delete new BlobKey(vsp.pictureKey)
      }
      
      vsp.pictureKey = blobKey getKeyString

      println(blobKey)
      val img = try {
        ImagesServiceFactory makeImageFromBlob blobKey
      } catch {
        case e=> {
          println("Faield with blobkey; " + blobKey)
          throw e
        }
      }
        
      val newImg = (ImagesServiceFactory getImagesService) applyTransform (
        ImagesServiceFactory makeCrop (0, 0, 1, 1), img)
      
      vsp.width = (newImg getWidth)
      vsp.height = (newImg getHeight)
      
      vsp.pictureUrl = url
      
      vsp.save()

      RedirectResponse("/blobs/uploadDone/")
    }
  }

  def uploadRedirect: View = withArg("synonymGroupKey") {
    (req, key) => 
      val synonymGroupKey = URLDecoder decode (key, "UTF-8")
      val blobKey = (blobstoreService getUploadedBlobs (req.originalRequest get)) get "blob"
      val pictureUrl = (ImagesServiceFactory getImagesService) getServingUrl blobKey

      val sg = SynonymGroup get (java.lang.Long parseLong synonymGroupKey)
      
      if (sg.pictureKey != null) { 
        blobstoreService delete new BlobKey(sg pictureKey)
      }

      sg.pictureKey = blobKey getKeyString

      sg.pictureUrl = pictureUrl
      
      sg.save()
      RedirectResponse("/blobs/uploadDone/")
  }
  
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

        "%s_%s.jpg" format (milltype, inout)
      }
    )
  }
    
  
  case class StudiedSubject(name: String, subjects: List[SynonymGroup])

  def millPart: View = {
    req => {
      val vsp = getVsp(req)
      val wordsAsync = if (vsp.words != null) Model.obj.async get(vsp words) else null
      val subjects = vsp subjects

      val studiedSubjects = if (subjects != null) { 
        subjects map { case Subject(name, keys) => (name, Model.obj.async get(keys toSeq)) } map { 
          case (name, vsps) => StudiedSubject(name, ((vsps get) values) toList)
        }
      } else null
          

      val words = if (wordsAsync != null) ((wordsAsync get) values) toList else null
      
      TemplateResponse(if (req isAjax) "main.millbrowser_ajax" else  "main.millbrowser", 
                       "vsp" -> vsp, "words" -> words, "subjects" -> studiedSubjects)
    }
  }
  
  def search: View = { 
    req => TemplateResponse("main.search", "seed" -> (req getArg "ord" getOrElse ""))
  }
}
