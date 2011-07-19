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

abstract class BaseRow[T](implicit m: Manifest[T]) { 
  def save() = { Model.obj.putOne(this); this }

  private def idField = 
    ((getClass getDeclaredFields) filterNot { 
      field => field.getDeclaredAnnotations filter { _.annotationType == classOf[javax.persistence.Id] } isEmpty
    } headOption) map { _ getName }

  private def id: Long  = ((idField map { getClass getMethod _ } map { 
    _ invoke this
  }).get.asInstanceOf[java.lang.Long]) longValue

  def getKey = new Key[T](m.erasure.asInstanceOf[Class[T]], id)
}

object Article extends BaseRowObj[Article] {
  def findArticle(word: String) =
    (Synonym findSynonym word) map { 
      synonym => Article get (synonym article)
    }
}

class Article extends BaseRow[Article] { 
  @Id var id: java.lang.Long = _
  @Indexed var path: String = _
  var mainSynonym: String = _
  var groupName: String = _
  var text: String = _
  var words: JList[Key[Synonym]] = _
  
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

class Synonym extends BaseRow[Synonym] { 
  @Id var number: java.lang.Long = _
  @Indexed var word: String = _
  @Indexed var sources: JList[String] = _
  var article: Key[Article] = _ 
  var pictureKey: String = _
  var pictureUrl: String = _
  
  override def toString = "%d - %s" format (number, word)

  def getArticle: Article = Article get article
}

object Model { 
  ObjectifyService.register(classOf[Article])
  ObjectifyService.register(classOf[Synonym])
  
  lazy val obj = ObjectifyService begin
}
