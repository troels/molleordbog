package org.bifrost.utils

import scala.collection.JavaConversions._

import com.google.appengine.api.memcache._
import org.bifrost.utils.U._

object Memcache { 
  def ms = MemcacheServiceFactory getMemcacheService
  
  var isOn = false

  def put[S, T](m: Map[S, T]) {
    if (isOn) {
      ms putAll m
    }
  }
  
  def get[S, T](key: S*): Map[S, T] = {
    if (isOn) { 
      (ms getAll key toMap).asInstanceOf[Map[S, T]]
    } else {
      Map()
    }
  }


  def delete(keys: AnyRef*) { 
    if (isOn) {
      ms deleteAll keys
    }
  }
}
