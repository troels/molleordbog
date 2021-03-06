package org.bifrost.utils.mapping

import org.bifrost.utils.http._
import org.bifrost.utils.U._
import scala.util.matching.Regex
import java.util.regex.Pattern
import Mapping._

import java.net.URLDecoder
abstract class MapperElement { 
  def matchup(req: Request, part: String): Option[Request]
}

case class MapperString(str: String) extends MapperElement {
  override def matchup(req: Request, uriPart: String): Option[Request] = 
    if (uriPart == str) Some(req) else None
}

class RegexMapperElement(name: String, regex: Regex) extends MapperElement{
  override def matchup(req: Request, uriPart: String) =  {
    uriPart match { 
      case regex(part) => safelyNullable(req.putRequestAttribute(name, part))
      case _ => None
    }
  }
}

case class MapperWord(name: String) extends RegexMapperElement(name, "^(.*)$" r)
case class MapperSlug(name: String) extends RegexMapperElement(name, "^([\\w_-]+)$" r)
case class MapperNumber(name: String) extends RegexMapperElement(name, "^(\\d+)$" r)
case class MapperList(name: String, elems: String*) extends RegexMapperElement(
  name,  ('(' + (elems map { Pattern quote _ } mkString "|") + ')') r)
                                                                               
object Mapping { 
  type MappingResult = Option[(Request, Request => Response)]
}

abstract class Mapping { 
  def apply(req: Request, uriParts: List[String]): MappingResult

  def apply(req: Request, uriParts: Option[List[String]]): MappingResult = 
    apply(req, uriParts getOrElse (URLDecoder decode (req.uri, "UTF-8") split "/" filter (! _.isEmpty) toList))

  def apply(req: Request): MappingResult = 
    apply(req, None)

  def |(mapping: Mapping) = new MappingSwitch(this, mapping)
}


object FrontpageMapping {
  def apply() = new MappingPathGenerator(List())
}

class MappingPath(pathMatcher: List[MapperElement], action: Mapping) extends Mapping {
  private def findBestMatch(req: Request, uriParts: List[String], 
                            matcherParts: List[MapperElement]): Option[(Request, List[String])] = {
    if (matcherParts isEmpty) return Some((req, uriParts))
    if (uriParts isEmpty) return None
    
    val uriHead :: uriRest = uriParts
    val matcherHead :: matcherRest = matcherParts

    matcherHead matchup (req, uriHead) flatMap (findBestMatch(_, uriRest, matcherRest))
  }
  
  override def apply(req: Request, uriParts: List[String]): MappingResult = {
    findBestMatch(req, uriParts, pathMatcher) flatMap  { case (req, newParts) => action(req, newParts) }
  }
}

class MappingSwitch(matchers: Mapping*) extends Mapping { 
  override def apply(req: Request, uriParts: List[String]): MappingResult = 
    (matchers foldLeft (None.asInstanceOf[MappingResult])) {
      (accum, action) => if (accum isDefined) accum else action(req, uriParts)
    }

  override def |(mapping: Mapping) = new MappingSwitch((matchers :+ mapping) : _*)
}

class MappingAction(func: Request => Response) extends Mapping {
  override def apply(req: Request, uriParts: List[String]) = 
    if (uriParts isEmpty) Some((req, func)) else None
}

class MappingPathGenerator(path: List[MapperElement]) { 
  def /(me: MapperElement) = new MappingPathGenerator(me :: path)
  def /(mapping: Mapping) = new MappingPath(path reverse, mapping)
  def ==>(action: Request => Response) = new MappingPath(path reverse, new MappingAction(action))
}

abstract class BaseMapping { 
  implicit def string2MapperElement(str: String) = new MapperString(str)
  implicit def mapperElement2MappingPath(mp: MapperElement): MappingPathGenerator = new MappingPathGenerator(List(mp))
  implicit def string2MappingPath(str: String): MappingPathGenerator = new MapperString(str)

  def slug(str: String) = new MapperSlug(str)
  def number(str: String) = new MapperNumber(str)

  val mappings: Mapping
  def apply(req: Request) = mappings(req)
}
