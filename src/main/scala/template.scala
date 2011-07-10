package org.bifrost.utils.templates

import java.io.File

import org.bifrost.utils.http._

import org.bifrost.counterfeiter.Counterfeiter
import org.bifrost.counterfeiter.Expression.ElementaryExpression

object OurCounterfeiter { 
  private lazy val machine = 
    Counterfeiter.loadFromDir(new File(getClass.getClassLoader.getResource("templates").getFile))
  
  def renderTemplate(name: String, lst: List[ElementaryExpression] = List(), 
                     map: Map[String, ElementaryExpression] = Map()) = 
     "<!doctype html>\n%s\n" format machine.renderTemplate(name, lst, map)
}


class TemplateResponse(templateName:String, args: (String, ElementaryExpression)*) extends 
      HtmlResponse(OurCounterfeiter.renderTemplate(templateName, map=args toMap))
