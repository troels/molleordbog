package org.bifrost.molleordbog.model

import java.util.{ List => JList }
import javax.persistence.{ Id, PrePersist }

import com.googlecode.objectify._
import com.googlecode.objectify.annotation._

import scala.collection.JavaConversions._

import org.bifrost.utils.U._
import Implicits._


import org.bifrost.utils.Memcache

object Implicits { 
  implicit def objectify2enhancer(ob: Objectify): ObjectifyEnhancer = new ObjectifyEnhancer(ob)
}

class ObjectifyEnhancer(ob: Objectify) { 
  def putOne[T <: BaseRow[_]](obj: T): Key[T] = {
    (putMany(obj) head) _1
  }

  def putMany[T <: BaseRow[_]](objs: T*): Map[Key[T], T] = {
    val res = (ob put (objs: _*)) toMap
    
    Memcache delete (res map { case (k, v) => k } toSeq :_*)
    
    res
  }

  def getOne[T <: BaseRow[_]](id: Key[T]): Option[T] = {
    ob getMany (Seq(id) : _*) get id
  }

  def getMany[T <: BaseRow[_]](keys: Key[T]*): Map[Key[T], T] = {
    val m: Map[Key[T], T] = Memcache get (keys: _*)
    val missing = keys filterNot (m contains _)

    if (missing nonEmpty) {
      val newR = ob get missing toMap

      Memcache put newR

      newR ++ m
    } else {
      m
    }
  }
}

abstract class BaseRowObj[T <: BaseRow[_]](implicit m: Manifest[T]) {
  def query: Query[T] = (Model obj) query (m.erasure.asInstanceOf[Class[T]])

  def get(key: String): T = 
    getOption(key) get
    
  def get(key: Long): T = 
    getOption(key) get

  def get(key: Key[T]): T = 
    getOption(key) get 
  
  def getOption(key: Key[T]): Option[T] =  
    get(Seq(key)) get key

  def getOption(key: Long): Option[T] = {
    val k = new Key(m.erasure.asInstanceOf[Class[T]], key)
    getOption(k)
  }

  def getOption(key: String): Option[T] = {
    val k = new Key(m.erasure.asInstanceOf[Class[T]], key) 
    getOption(k)
  }    

  def get(keys: Seq[Key[T]]): Map[Key[T], T] = 
    (Model obj) getMany (keys :_*) toMap
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
           filter ("word <= ", prefix + "\uffff") 
           limit 50)
  
  def findSynonym(word: String): Option[Synonym] = 
    (query filter ("word = ", word) toList) headOption

  def apply(): Synonym = new Synonym
}

class Synonym extends BaseRow[Synonym] with java.io.Serializable {
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

class SynonymGroup extends BaseRow[SynonymGroup] with java.io.Serializable { 
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
    if (synonyms != null) (Synonym get synonyms values) toList else List()

}

case class Subject(name: String, words: Array[Key[SynonymGroup]])
case class Excision(x: Int, y: Int, width: Int, height: Int, picture: Key[VisualSearchPicture])

object VisualSearchPicture extends BaseRowObj[VisualSearchPicture] { 
  def apply() = new VisualSearchPicture()
}

class VisualSearchPicture extends BaseRow[VisualSearchPicture] with java.io.Serializable { 
  @Id var pictureName: String = _
  
  var pictureKey: String = _
  var pictureUrl: String = _
  
  var width: Int = -1
  var height: Int = -1
  var words: JList[Key[SynonymGroup]] = _
  
  @Serialized var subjects: Array[Subject] = _
  @Serialized var excisions: Array[Excision] = _
  
  override def toString: String = 
    "%s - %s - %s" format (pictureName, 
                           if (subjects != null) (subjects toList) else null,
                           if (excisions != null) (excisions toList) else null)
} 

object Source extends BaseRowObj[Source] { 
  def apply() = new Source()
}

class Source extends BaseRow[Source]  { 
  @Id var id: java.lang.Long = _
    
  var name: String = _
  var text: String = _
  
  var pdfKey: String = _

  var pictureKey: String = _
  var pictureUrl: String = _
}

object Page extends BaseRowObj[Page] {
  def apply() = new Page()
}

class Page extends BaseRow[Page] { 
  @Id var path: String = _
  
  @PrePersist def onSave(obj: Objectify) { 
    path = path toLowerCase
  }

  var title: String = _
  var html: String = _

  var pictureKey: String = _
  var pictureUrl: String = _
}

object Model { 
  ObjectifyService.register(classOf[Page])
  ObjectifyService.register(classOf[SynonymGroup])
  ObjectifyService.register(classOf[Synonym])
  ObjectifyService.register(classOf[VisualSearchPicture])
  ObjectifyService.register(classOf[Source])
  
  var isImport: Boolean = true

  def obj = ObjectifyService begin (new ObjectifyOpts().setGlobalCache(isImport))
}
