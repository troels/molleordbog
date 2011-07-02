package org.bifrost.molleordbog.model

import java.util.{ List => JList }
import javax.persistence.Id

import com.googlecode.objectify._
import com.googlecode.objectify.annotation._

import scala.collection.JavaConversions._

import org.bifrost.utils.U._
import Implicits._


class ObjectifyEnhancer(ob: Objectify) { 
  def putOne(obj: AnyRef) = ob put (Seq(obj) : _*)
  def putMany(objs: AnyRef*) = ob put (objs : _*)
  def getOne[T](cls: Class[T], id: Long): Option[T] = ob get (cls, Seq(id) : _*) get id
  def getMany[T](cls: Class[T], objs: Long*): Map[Long, T] = new JMapWrapper((ob get (cls, objs: _*))) toMap
}

object Implicits { 
  implicit def objecify2enhancer(ob: Objectify) = new ObjectifyEnhancer(ob)
}

abstract class BaseRowObj[T](implicit m: Manifest[T]) {
  def get(id: Long):Option[T] = Model.obj.getOne[T](m.erasure.asInstanceOf[Class[T]], id)
}

abstract class BaseRow { 
  def save() = { Model.obj.putOne(this); this }
}

object Article extends BaseRowObj[Article]

class Article extends BaseRow { 
  @Id var id: java.lang.Long = _
  var groupName: String = _
  var mainSynonym: String = _
  var text: String = _
  var words: JList[Key[Synonym]] = _
}

object Synonym extends BaseRowObj[Synonym]

class Synonym extends BaseRowObj[Synonym] { 
  @Id var number: java.lang.Long = _
  @Indexed var word: String = _
  @Indexed var sources: JList[String] = _
  var article: Key[Article] = _ 

  override def toString = "%d - %s" format (number, word)
}

object Model { 
  ObjectifyService.register(classOf[Article])
  ObjectifyService.register(classOf[Synonym])
  lazy val obj = ObjectifyService begin
}
