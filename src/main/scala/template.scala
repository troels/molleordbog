package org.bifrost.utils.templates

import java.io.File

import org.bifrost.utils.http._

import org.bifrost.counterfeiter.Counterfeiter
import org.bifrost.counterfeiter.Expression.{UntypedExpression, ElementaryExpression }

object OurCounterfeiter { 
  private def machine =
    Counterfeiter.loadFromDir(new File(getClass.getClassLoader.getResource("templates").getFile))
  
  def renderTemplate(name: String, lst: List[ElementaryExpression] = List(), 
                     map: Map[String, ElementaryExpression] = Map()) = 
     "<!doctype html>\n%s\n" format machine.renderTemplate(name, lst, map)
}


object TemplateResponse { 
  def apply(templateName: String, args: (String, AnyRef)*) = 
    new TemplateResponse(templateName, args : _*)
}

class TemplateResponse(templateName:String, args: (String, AnyRef)*) extends 
      HtmlResponse(OurCounterfeiter.renderTemplate(
        templateName, 
        map=args map {case (k, v) => k -> new UntypedExpression(v) } toMap))
