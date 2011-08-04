package org.bifrost.utils

import org.bifrost.counterfeiter.Expression.{ ElementaryExpression, BasicExpression, ElementaryListExpression }

import com.google.gson.GsonBuilder
import com.google.gson.JsonSerializer
import com.google.gson.JsonElement
import com.google.gson.JsonSerializationContext

import java.lang.reflect.Type

object U {
  object SerializeList extends JsonSerializer[List[_]] {
    override def serialize(src: List[_], typeOfSrc: Type, context: JsonSerializationContext): JsonElement =
      context serialize (src toArray)
  }

  def gson = new GsonBuilder().registerTypeHierarchyAdapter(classOf[List[_]], SerializeList).create()

  
  def safelyNullable[A](v: A): Option[A] = v match { 
    case null => None 
    case o => Some(o)
  }

  class ListWrapper[A](seq: List[A]) { 
    def findFirst[B](f: A => Option[B]): Option[(B, List[A])] = {
      def inner(s: List[A]): Option[(B, List[A])] = 
        s match {
          case head :: tail => 
            f(head) match {
              case Some(x) => Some((x, tail))
              case None => inner(tail)
            }
          case Nil => None
        }
      inner(seq)
    }
  }

  implicit def seq2seqwrapper[A](seq: List[A]) = new ListWrapper(seq)

  implicit def string2elementaryExpression(str: String) = 
    new BasicExpression[String](str)
  
  implicit def int2elementaryExpression(int: Int) = 
    new BasicExpression[Int](int)

  implicit def listOfString2elementaryExpression(lst: List[String]) = 
    new ElementaryListExpression(lst map { string2elementaryExpression(_) } : _ *)

  implicit def listOfInt2elementaryExpression(lst: List[Int]) = 
    new ElementaryListExpression(lst map { int2elementaryExpression(_) } : _ *)
}
