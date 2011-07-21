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

import net.liftweb.json.JsonDSL._

import java.net.URLEncoder

object Utils { 
  lazy val collator = Collator.getInstance(new Locale("da", "dk"))
}

object MolleOrdbogMappings extends BaseMapping {
  lazy val mappings: Mapping = 
    (FrontpageMapping() ==> Views.search) |
    ("soeg" ==> Views.search) |
    ("ordbog" / "autocomplete" ==> Views.autocomplete) | 
    ("ordbog" / "opslag" ==>  Views.lookup) |
    ("blobs" / (
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
      RequestErrorResponse("Missing argument: %s" format arg)
    }
  
  def withArg(arg0: String, arg1: String)(f: (Request, String, String) => Response)(req: Request): Response =
    req getArg arg0 map { 
      argVal0 => withArg(arg1)( (req, argVal1) => f(req, argVal0, argVal1) )(req) 
    } getOrElse {
      RequestErrorResponse("Missing argument: %s" format arg0)
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
  
  def autocomplete: View = withArg("term") {
    (req, word) => 
      val synonyms = (Synonym findWithPrefix word toList) map (_ word) take 10
      
      JSONResponse(synonyms)
  }

  def uploadUrl: View = {
    _ => TextResponse(blobstoreService createUploadUrl "/blobs/uploadRedirect/")
  }

  def uploadRedirect: View = withArg("synonymGroupKey") {
    (req, synonymGroupKey) => 
      val blobKey = (blobstoreService getUploadedBlobs (req.originalRequest get)) get "blob"
      val pictureUrl = (ImagesServiceFactory getImagesService) getServingUrl blobKey

      val sg = SynonymGroup get (java.lang.Long parseLong synonymGroupKey)
      
      if (sg.pictureKey != null) { 
        blobstoreService delete new BlobKey(sg.pictureKey)
      }

      sg.pictureKey = blobKey getKeyString

      sg.pictureUrl = pictureUrl
      
      sg.save()
      RedirectResponse("/blobs/uploadDone/")
  }
  
  def uploadDone: View = { _ => TextResponse("Upload complete") }

  def search: View = { req => 
    val ord = req getArg "ord" getOrElse ""  
    TemplateResponse("main.search", "seed" -> ord)
  }
}
