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
  def putMany[T](objs: BaseRow[T]*) = ob put (objs: _*)

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

object Synonym extends BaseRowObj[Synonym] { 
  def findWithPrefix(prefix: String): Query[Synonym] =
    (query filter ("word >= ", prefix + "\u0000") 
           filter ("word <= ", prefix + "\uffff"))
  
  def findSynonym(word: String): Option[Synonym] = 
    (query filter ("word = ", word) toList) headOption

  def apply(): Synonym = new Synonym
}

class Synonym extends BaseRow[Synonym] {
  @Id var id: java.lang.Long = _
  @Indexed var word: String = _
  @Indexed var sources: JList[String] = _
  var synonymGroup: Key[SynonymGroup] = _
  var artificial: Boolean = _
  
  override def toString = "%s - %s" format (word, sources mkString ", ")

  def getSynonymGroup: SynonymGroup = SynonymGroup get synonymGroup 
}

object SynonymGroup extends BaseRowObj[SynonymGroup] {
  def findSynonymGroup(word: String) = 
    Synonym findSynonym word map { 
      syn => SynonymGroup get (syn synonymGroup)
    }

  def apply(): SynonymGroup = new SynonymGroup
}

class SynonymGroup extends BaseRow[SynonymGroup] { 
  @Id var id: java.lang.Long = _
  @Indexed var number: java.lang.Long = _

  var text: String = _
  var canonicalWord: String = _
  var path: String = _
  var synonyms: JList[Key[Synonym]] = _

  var pictureKey: String = _
  var pictureUrl: String = _

  override def toString = "%d - %s" format (number, canonicalWord)

  def getSynonyms: List[Synonym] = 
    if (synonyms != null) ((Synonym get synonyms) values) toList else List()

}

object Model { 
  ObjectifyService.register(classOf[SynonymGroup])
  ObjectifyService.register(classOf[Synonym])
  
  lazy val obj = ObjectifyService begin
}
