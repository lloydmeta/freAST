package freast.scalaz

import org.scalatest.{FunSpec, Matchers}

import scala.collection.immutable
import scalaz.Free
import scalaz.Id._

/**
  * Created by Lloyd on 12/21/16.
  *
  * Copyright 2016
  */
@free
trait KVStore {
  type KVStoreF[A] = Free[GrammarADT, A]
  sealed trait GrammarADT[A]

  def put[T](key: String, value: T): KVStoreF[Unit]
  def get[T](key: String): KVStoreF[Option[T]]
  def delete(key: String): KVStoreF[Unit]

  def update[T](key: String, f: T => T): KVStoreF[Unit] =
    for {
      vMaybe <- get[T](key)
      _      <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
    } yield ()
}

class ScalazFreeSpec extends FunSpec with Matchers {

  val impureInterpreter = new KVStore.Interp[Id] {
    private var kvs = immutable.Map.empty[String, Any]
    def get[T](key: String): Id[Option[T]] = {
      kvs.get(key).map(_.asInstanceOf[T])
    }
    def put[T](key: String, value: T): Id[Unit] = {
      kvs += key -> value
    }
    def delete(key: String): Id[Unit] = {
      kvs -= key
    }
  }

  describe("smoke test") {

    import KVStore.ops._

    it("should work") {

      val program: KVStoreF[Option[Int]] =
        for {
          _ <- put("wild-cats", 2)
          _ <- update[Int]("wild-cats", _ + 12)
          _ <- put("tame-cats", 5)
          n <- get[Int]("wild-cats")
        } yield n

      val r = impureInterpreter.run(program)
      r shouldBe Some(14)

    }

  }

}
