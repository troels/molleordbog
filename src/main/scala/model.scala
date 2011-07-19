package org.bifrost.molleordbog.model

import java.util.{ List => JList }
import javax.persistence.Id

import com.googlecode.objectify._
import com.googlecode.objectify.annotation._

import scala.collection.JavaConversions._

import org.bifrost.utils.U._
import Implicits._


class ObjectifyEnhancer(ob: Objectify) { 
  def putOne[T](obj: T): Key[T] = (ob put (Seq(obj) : _*)).keySet.iterator.next
  def putMany(objs: AnyRef*) = ob put (objs: _*)
  def getOne[T](cls: Class[T], id: Long): Option[T] = ob get (cls, Seq(id) : _*) get id
  def getMany[T](cls: Class[T], objs: Long*): Map[Long, T] = new JMapWrapper((ob get (cls, objs: _*))) toMap
}

object Implicits { 
  implicit def objectify2enhancer(ob: Objectify) = new ObjectifyEnhancer(ob)
}

abstract class BaseRowObj[T](implicit m: Manifest[T]) {
  def query: Query[T] = (Model obj) query (m.erasure.asInstanceOf[Class[T]])
  def get(key: String) = (Model obj) get (new Key(m.erasure.asInstanceOf[Class[T]], key))
  def get(key: Long) = (Model obj) get (new Key(m.erasure.asInstanceOf[Class[T]], key))
  def get(key: Key[T]) = (Model obj) get key
  def get(keys: Seq[Key[T]]) = (Model obj) get keys toMap
}

abstract class BaseRow { 
  def save() = { Model.obj.putOne(this); this }
}

object Article extends BaseRowObj[Article] {
  def findArticle(word: String) =
    (Synonym findSynonym word) map { 
      synonym => Article get (synonym article)
    }
}

class Article extends BaseRow { 
  @Id var id: java.lang.Long = _
  @Indexed var path: String = _
  var mainSynonym: String = _
  var groupName: String = _
  var text: String = _
  var words: JList[Key[Synonym]] = _
  var pictures: JList[String] = _

  def getSynonyms: List[Synonym] = 
    if (words != null) (Synonym get words).values toList else List()
  
  override def toString = "Article: %s" format groupName
}
  
object Synonym extends BaseRowObj[Synonym] { 
  def findWithPrefix(prefix: String): Query[Synonym] =
    (query filter ("word >= ", prefix + "\u0000") 
           filter ("word <= ", prefix + "\uffff"))
  
  def findSynonym(word: String): Option[Synonym] = 
    (query filter ("word = ", word) toList) headOption
}

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
