package org.bifrost.utils

object U {
  implicit def safelyNullable[A](v: A): Option[A] = v match { 
    case null => None 
    case o => Some(o)
  }

  class ListWrapper[A](seq: List[A]) { 
    def findFirst[B](f: A => Option[B]): Option[(B, List[A])] = {
      def inner(s: List[A]): Option[(B, List[A])] = if (s isEmpty) None else f(s head) match { 
        case Some(x) => Some(x, s tail) 
        case None => inner(s tail)
      }
      inner(seq)
    }
  }

  implicit def seq2seqwrapper[A](seq: List[A]) = new ListWrapper(seq)
}
