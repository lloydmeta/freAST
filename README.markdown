# Freast [![Build Status](https://travis-ci.org/lloydmeta/freAST.svg?branch=master)](https://travis-ci.org/lloydmeta/freAST)

**WIP**

Freast is a Scalameta macro-based lib for easily building Free-monad based AST. It is a port of [Freasy-monad](https://github.com/Thangiee/Freasy-Monad/) to
the newer Scalameta-based toolkit.

Learning Scalameta was a big motivation, and doing this port was similar to taking apart a wall clock and putting it into
another sleeker case.

## Goals

- Integrate well with IntelliJ's Scalameta support (auto-expansion and auto-complete)
- Make it easy to use Free Monads
- Compatible with Cats and Scalaz

## SBT

In `build.sbt`
```scala
libraryDependencies += "com.beachape" %% "freast-cats" % "0.1.0-SNAPSHOT" // or freast-scalaz if you swing that way


// Additional ceremony for using Scalameta macro annotations

resolvers += Resolver.url(
  "scalameta",
  url("http://dl.bintray.com/scalameta/maven"))(Resolver.ivyStylePatterns)

// A dependency on macro paradise is required to both write and expand
// new-style macros.  This is similar to how it works for old-style macro
// annotations and a dependency on macro paradise 2.x.
addCompilerPlugin(
  "org.scalameta" % "paradise" % "4.0.0.142" cross CrossVersion.full)

scalacOptions += "-Xplugin-require:macroparadise"
```

## Example

```scala
import cats.Id
import cats.free.Free

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

// Then

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
```

The `@free` macro expands to this, which includes `Inject` for composing different ADT grammars.:

```scala
object KVStore {
  import cats._
  import cats.free._
  import scala.language.higherKinds
  sealed trait GrammarADT[A]
  object GrammarADT {
    case class Put[T](key: String, value: T) extends GrammarADT[Unit]
    case class Get[T](key: String) extends GrammarADT[Option[T]]
    case class Delete(key: String) extends GrammarADT[Unit]
  }
  object ops {
    type KVStoreF[A] = Free[GrammarADT, A]
    def put[T](key: String, value: T): KVStoreF[Unit] = injectOps.put[GrammarADT, T](key, value)
    def get[T](key: String): KVStoreF[Option[T]] = injectOps.get[GrammarADT, T](key)
    def delete(key: String): KVStoreF[Unit] = injectOps.delete[GrammarADT](key)
    def update[T](key: String, f: T => T): KVStoreF[Unit] = injectOps.update[GrammarADT, T](key, f)
  }
  object injectOps {
    def put[F[_], T](key: String, value: T)(implicit I: Inject[GrammarADT, F]): Free[F, Unit] = Free.liftF(I.inj(GrammarADT.Put(key, value)))
    def get[F[_], T](key: String)(implicit I: Inject[GrammarADT, F]): Free[F, Option[T]] = Free.liftF(I.inj(GrammarADT.Get(key)))
    def delete[F[_]](key: String)(implicit I: Inject[GrammarADT, F]): Free[F, Unit] = Free.liftF(I.inj(GrammarADT.Delete(key)))
    def update[F[_], T](key: String, f: T => T)(implicit I: Inject[GrammarADT, F]): Free[F, Unit] = get[F, T](key).flatMap(vMaybe => vMaybe.map(v => put[F, T](key, f(v))).getOrElse(Free.pure(())).map(_ => ()))
  }
  class Injects[F[_]](implicit I: Inject[GrammarADT, F]) {
    def put[T](key: String, value: T): Free[F, Unit] = injectOps.put[F, T](key, value)(I)
    def get[T](key: String): Free[F, Option[T]] = injectOps.get[F, T](key)(I)
    def delete(key: String): Free[F, Unit] = injectOps.delete[F](key)(I)
    def update[T](key: String, f: T => T): Free[F, Unit] = injectOps.update[F, T](key, f)(I)
  }
  object Injects { implicit def injectOps[F[_]](implicit I: Inject[GrammarADT, F]): Injects[F] = new Injects[F] }
  trait Interp[M[_]] {
    import ops._
    val interpreter = new ~>[GrammarADT, M] {
      def apply[A](fa: GrammarADT[A]): M[A] = fa match {
        case GrammarADT.Put(key, value) => put(key, value)
        case GrammarADT.Get(key) => get(key)
        case GrammarADT.Delete(key) => delete(key)
      }
    }
    def run[A](op: KVStoreF[A])(implicit m: Monad[M]): M[A] = op.foldMap(interpreter)
    def put[T](key: String, value: T): M[Unit]
    def get[T](key: String): M[Option[T]]
    def delete(key: String): M[Unit]
  }
}
```