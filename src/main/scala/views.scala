package org.bifrost.molleordbog.views

import org.bifrost.utils.mapping._
import org.bifrost.utils.http._
import org.bifrost.molleordbog.model._
import org.bifrost.utils.U._

import java.text.Collator
import java.util.Locale

import scala.collection.JavaConversions._

object Utils { 
  lazy val collator = Collator.getInstance(new Locale("da", "dk"))
}

object MolleOrdbogMappings extends BaseMapping {
  lazy val mappings: Mapping = 
    ("autocomplete" ==> Views.autoComplete)
}


object Views { 
  type View = Request => Response

  def withArg(arg: String)(f: (Request, String) => Response)(req: Request): Response =
    req getArg arg map { 
      argVal => f(req, argVal)
    } getOrElse {
      HttpRequestErrorResponse("Missing argument: %s" format arg)
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
}
