package org.bifrost.utils

object U {
  implicit def safelyNullable[A](v: A): Option[A] = v match { 
    case null => None 
    case o => Some(o)
  }
}
