package freast.cats

import freast.internal

import scala.annotation.compileTimeOnly
import scala.collection.immutable._
import scala.meta._


/**
  * Created by Lloyd on 12/20/16.
  *
  * Copyright 2016
  */

@compileTimeOnly("@free Free Monad annotation failed to expand. Perhaps you forgot to add Macro Paradise?")
class free extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    internal.Shared.freeMonadise(defn, Seq(q"import cats._", q"import cats.free._"))
  }

}