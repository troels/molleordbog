package org.bifrost.utils.mapping

import org.bifrost.utils.http._
import org.bifrost.utils.U._
import scala.util.matching.Regex
import Mapping._

abstract class MapperElement { 
  def matchup(req: Request, part: String): Option[Request]
}

case class MapperString(str: String) extends MapperElement {
  override def matchup(req: Request, uriPart: String): Option[Request] = 
    if (uriPart == str) req else None
}

class RegexMapperElement(name: String, regex: Regex) extends MapperElement{
  override def matchup(req: Request, uriPart: String) = 
    uriPart match { 
      case regex(part) => req.putRequestAttribute(name, part)
      case _ => None
    }
}

case class MapperSlug(name: String) extends RegexMapperElement(name, "^([\\w_-]+)$".r)
case class MapperNumber(name: String) extends RegexMapperElement(name, "^(\\d+)$".r)

object Mapping { 
  type MappingResult = Option[(Request, Request => Response)]
}

abstract class Mapping { 
  def apply(req: Request, uriParts: List[String]): MappingResult

  def apply(req: Request, uriParts: Option[List[String]]): MappingResult = 
    apply(req, uriParts getOrElse (req.uri split "/" filter (! _.isEmpty) toList))

  def apply(req: Request): MappingResult = 
    apply(req, None)

  def |(mapping: Mapping) = new MappingSwitch(this, mapping)
}


object FrontpageMapping {
  def apply() = new FrontpageMapping()
}

class FrontpageMapping()

  
class MappingPath(pathMatcher: List[MapperElement], action: Mapping) extends Mapping {
  private def findBestMatch(req: Request, uriParts: List[String], 
                            matcherParts: List[MapperElement]): Option[(Request, List[String])] = {
    if (matcherParts isEmpty) return (req, uriParts)
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
    if (uriParts isEmpty) (req, func) else None
}

class MappingPathGenerator(path: List[MapperElement]) { 
  def /(me: MapperElement) = new MappingPathGenerator(me :: path)
  def /(mapping: MappingSwitch) = new MappingPath(path reverse, mapping)
  def ==>(action: Request => Response) = new MappingPath(path reverse, new MappingAction(action))
}

abstract class BaseMapping { 
  implicit def string2MapperElement(str: String) = new MapperString(str)
  implicit def mapperElement2MappingPath(mp: MapperElement): MappingPathGenerator = new MappingPathGenerator(List(mp))
  implicit def string2MappingPath(str: String): MappingPathGenerator = new MapperString(str)
  implicit def frontpage2Mapping(fp: FrontpageMapping): MappingPathGenerator = new MappingPathGenerator(List())

  def slug(str: String) = new MapperSlug(str)
  def number(str: String) = new MapperNumber(str)

  val mappings: Mapping
  def apply(req: Request) = mappings(req)
}
