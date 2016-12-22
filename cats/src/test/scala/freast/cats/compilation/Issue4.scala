package freast.cats.compilation

import cats.free.Free
import freast.cats.free

/**
  * Taken from https://github.com/Thangiee/Freasy-Monad/blob/master/core/shared/src/test/scala/compilation/Issue4.scala
  */
@free
trait FilterStore1 {
  type FilterStoreF[A] = Free[FilterStoreGrammar, A]
  sealed trait FilterStoreGrammar[A]

  def getAll[T]: FilterStoreF[List[T]]

  // fix: expanded code does not add type param F to method's rhs in Injects class in some cases such as this one
  def getCount: FilterStoreF[Int] = getAll[Any].map(_.length).map(identity)
}

@free
trait FilterStore2 {
  type FilterStoreF[A] = Free[FilterStoreGrammar, A]
  sealed trait FilterStoreGrammar[A]

  // fix: empty-parentheses cause "too many arguments" error for methods in Injects class
  def getAll[T](): FilterStoreF[List[T]]
  def getCount: FilterStoreF[Int] = getAll[Any]().map(_.length)
}
