package freast.cats.compilation

import cats.Id
import cats.free.Free
import freast.cats.free
import scala.collection.immutable

/**
  * Taken from https://github.com/Thangiee/Freasy-Monad/blob/master/core/shared/src/test/scala/compilation/MultiParamsLists.scala
  */
object MultiParamLists extends App {
  @free
  trait KVStore {
    type KVStoreF[A] = Free[GrammarADT, A]
    sealed trait GrammarADT[A]

    def put[T](key: String)(value: T): KVStoreF[Unit]
    def get[T](key: String): KVStoreF[Option[T]]

    def update[T](key: String)(f: T => T): KVStoreF[Unit] =
      for {
        vMaybe <- get[T](key)
        _      <- vMaybe.map(v => put[T](key)(f(v))).getOrElse(Free.pure(()))
      } yield ()
  }

  import KVStore.ops._

  def program: KVStoreF[Option[Int]] =
    for {
      _ <- put("wild-cats")(2)
      _ <- update[Int]("wild-cats")(_ + 12)
      n <- get[Int]("wild-cats")
    } yield n

  val idInterpreter = new KVStore.Interp[Id] {
    var kvs = immutable.Map.empty[String, Any]
    def get[T](key: String): Id[Option[T]] = {
      println(s"get($key)")
      kvs.get(key).map(_.asInstanceOf[T])
    }
    def put[T](key: String)(value: T): Id[Unit] = {
      kvs += key -> value
    }
  }
  val resId: Id[Option[Int]] = idInterpreter.run(program)
}
