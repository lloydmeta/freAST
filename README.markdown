# Freast [![Build Status](https://travis-ci.org/lloydmeta/freAST.svg?branch=master)](https://travis-ci.org/lloydmeta/freAST)

**WIP**

Freast is a Scalameta macro-based lib for easily building Free-monad based AST. It is a port of []Freasy-monad](https://github.com/Thangiee/Freasy-Monad/) to
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