package org.bifrost.molleordbog.views

import org.bifrost.utils.mapping._
import org.bifrost.utils.http._
import org.bifrost.molleordbog.model._
import org.bifrost.utils.U._

import java.text.Collator
import java.util.Locale

import scala.collection.JavaConversions._
import com.google.appengine.api.blobstore.BlobstoreServiceFactory
import com.google.appengine.api.blobstore.BlobKey

object Utils { 
  lazy val collator = Collator.getInstance(new Locale("da", "dk"))
}

object MolleOrdbogMappings extends BaseMapping {
  lazy val mappings: Mapping = 
    ("autocomplete" ==> Views.autoComplete) | 
    ("blobs" / (("uploadUrl" ==> Views.uploadUrl) |
                ("uploadRedirect" ==> Views.uploadRedirect) |
                ("uploadDone" ==> Views.uploadDone) | 
                ("removeBlobs" ==> Views.removeBlobs)))
}


object Views { 
  type View = Request => Response

  def blobstoreService = BlobstoreServiceFactory getBlobstoreService
  def withArg(arg: String)(f: (Request, String) => Response)(req: Request): Response =
    req getArg arg map { 
      argVal => f(req, argVal)
    } getOrElse {
      HttpRequestErrorResponse("Missing argument: %s" format arg)
    }
  
  def withArg(arg0: String, arg1: String)(f: (Request, String, String) => Response)(req: Request): Response =
    req getArg arg0 map { 
      argVal0 => withArg(arg1)( (req, argVal1) => f(req, argVal0, argVal1) )(req) 
    } getOrElse {
      HttpRequestErrorResponse("Missing argument: %s" format arg0)
    }

  def autoComplete: View = withArg("word") {
    (req, word) => 
      val synonyms = (Synonym findWithPrefix word toList) map (_ word) sortWith { (a, b) =>
        Utils.collator.compare(a toLowerCase, b toLowerCase) == -1 } 
      
      TextResponse(synonyms mkString "\n")
  }

  def showWord: View = withArg("word") {
    (req, word) => 
      (Article findArticle word) map { 
        article => {
          val synonyms = article getSynonyms 
          
          TextResponse("hello")
        }
      } getOrElse HttpRedirectResponse("/")
  }

  def uploadUrl: View = {
    _ => TextResponse(blobstoreService createUploadUrl "/blobs/uploadRedirect/")
  }

  def uploadRedirect: View = withArg("articleKey") {
    (req, articleKey) => 
      val blobKey = (blobstoreService getUploadedBlobs (req.originalRequest get)) get "blob"
       
      val article = Article get (java.lang.Long parseLong articleKey)
      article.pictures = article.pictures match {
        case null => List(blobKey getKeyString)
        case lst => (blobKey getKeyString) :: (lst toList)
      }
      article.save()
      HttpRedirectResponse("/blobs/uploadDone/")
  }
  
  def uploadDone: View = { _ => TextResponse("Upload complete") }

  def removeBlobs: View = withArg("articleKey") {
    (req, articleKey) => 
      val article = Article get (java.lang.Long parseLong articleKey)
      article.pictures match { 
        case null => 
        case o => blobstoreService delete (o map { new BlobKey(_) } toList : _*)
      }
    TextResponse("Success")
  }
}
