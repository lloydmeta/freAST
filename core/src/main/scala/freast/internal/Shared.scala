package freast.internal

import scala.meta._
import scala.collection.immutable._
import scala.meta.Defn.{Class, Object, Trait}

object Shared {

  def freeMonadise(tree: Tree, imports: Seq[Import]): Stat = {
    tree match {
      case q"..$mods trait $tname[..$tparams] extends $template" => {
        val typedContext = new TypedContext(template)
        import typedContext._

        ensureSoundMembers()

        // Extract common variables
        val abstractMembers = getAbstractMembers(template)
        if (abstractMembers.size <= 0) {
          abort(s"Did Not find any abstract members with $typeAlias return type. Add some.")
        }
        val concreteMembers            = getConcreteMembers(template)

        val (liftedOps, liftedOpsRefs) = generateLiftedOps(abstractMembers)
        val (concreteOps, concreteOpsRef, concreteNonOps, concreteNonOpsRef) =
          generateNewConcreteMembers(concreteMembers, liftedOps)

        val opsObj = {
          q"""
            object ops {
              $typeAlias
              ..$concreteNonOpsRef
              ..$liftedOpsRefs
              ..$concreteOpsRef
            }
          """
        }
        val injectOpsObj = {
          q"""
            object injectOps {
              ..$concreteNonOps
              ..$liftedOps
              ..$concreteOps
            }
           """
        }
        val (injectClass, injectObject) =
          generateInjectClassAndObject(liftedOps, concreteOps, concreteNonOpsRef)
        val objectWithCaseClasses =
          generateObjectContainingCaseClasses(abstractMembers)
        val interpreterTrait =
          generateInterpreterTrait(abstractMembers)
        val genCompanionObj =
          q"""
             ..$mods object ${Term.Name(tname.value)} {
              ..$imports
              import scala.language.higherKinds

              $sealedFreeTrait
              $objectWithCaseClasses
              $opsObj
              $injectOpsObj
              $injectClass
              $injectObject
              $interpreterTrait
            }
           """
        // print(genCompanionObj.syntax)

        // Sadly, the trait cannot simply disappear during macro expansion, so we'll just keep it around but seal it
        q"""
        ..$mods sealed trait $tname

        $genCompanionObj
        """
      }
      case _ => abort("We only work on traits")
    }
  }

  /**
    * Class that holds a few stable values so we don't need to pass them everywhere.
    */
  private class TypedContext(template: Template) {

    val (typeAlias, freeSType) = getFreeTypeAlias(template)
    val sealedFreeTrait: Trait = getSealedFreeTrait(template, freeSType)

    private val implicitInject = Seq(param"implicit I: Inject[${sealedFreeTrait.name}, F]")
    private val F              = tparam"F[_]"

    def getFreeTypeAlias(template: Template): (Defn.Type, Type.Name) = {
      val freeTypeAliases = template.stats.flatMap(_.collectFirst {
        case typedef @ q"..$mods type $tname[..$tparams] = Free[$s, $_]" => (typedef, s)
      })

      freeTypeAliases match {
        case Some((typedef: Defn.Type, s: Type.Name)) => (typedef, s)
        // IntelliJ uses this for some reason
        case Some((typedef: Defn.Type, s: Type.Select)) =>
          (typedef, s.name)
        case _ => {
          abort(s"There should be 1 and only 1 type alias for Free, found None")
        }
      }
    }

    def getSealedFreeTrait(template: Template, freeSType: Type.Name): Defn.Trait = {
      val sealedFreeTraits = template.stats.flatMap(_.collectFirst {
        case cd @ q"sealed trait $name[..$_]" if name.value == freeSType.value => cd
      })
      sealedFreeTraits match {
        case Some(cd: Defn.Trait)=> cd
        case _ =>
          abort(s"There should be 1 and only 1 sealed ADT class, found None")
      }
    }

    def ensureSoundMembers(): Unit = {
      for {
        s  <- template.stats
        st <- s
      } {
        st match {
          case d @ Defn.Def(mods, _, _, _, _, _) if mods.exists(e => e == Mod.Private(Name.Anonymous()) || e == Mod.Protected(Name.Anonymous())) => {
            abort(s"Please use only package private or protected: ${d.syntax}")
          }
          case v @ Defn.Val(mods, _, _, _) if mods.exists(e => e == Mod.Private(Name.Anonymous()) || e == Mod.Protected(Name.Anonymous())) => {
            abort(s"Please use only package private or protected: ${v.syntax}")
          }
          case d @ Decl.Def(mods, _, _, _, _) if mods.exists(e => e == Mod.Private(Name.Anonymous()) || e == Mod.Protected(Name.Anonymous())) => {
            abort(s"Please use only package private or protected: ${d.syntax}")
          }
          case v @ Decl.Val(mods, _, _) if mods.exists(e => e == Mod.Private(Name.Anonymous()) || e == Mod.Protected(Name.Anonymous())) => {
            abort(s"Please use only package private or protected: ${v.syntax}")
          }

          case v @ Defn.Def(_, _, _, _, None, _) =>
            abort(s"Return type must be explicitly stated for $v")
          case v @ Defn.Val(_, _, None, _) =>
            abort(s"Return type must be explicitly stated for $v")

          // <-- For IntelliJ
          case d @ Decl.Def(_, _, _, _, retType: Type.Select)
              if retType.name.value != typeAlias.name.value =>
            abort(
              s"Abstract def needs to have return type ${typeAlias.name}[...], otherwise, make it non-abstract: ${System
                .lineSeparator()} ${d.syntax}")
          case v @ Decl.Val(_, _, retType: Type.Select)
              if retType.name.value != typeAlias.name.value =>
            abort(
              s"Abstract val needs to have return type ${typeAlias.name}[...], otherwise, make it non-abstract: ${System
                .lineSeparator()} ${v.syntax}")
          // For IntelliJ -->

          case d @ Decl.Def(_, _, _, _, retType: Type.Name)
              if retType.value != typeAlias.name.value =>
            abort(
              s"Abstract def needs to have return type ${typeAlias.name}[...], otherwise, make it non-abstract: ${System
                .lineSeparator()} ${d.syntax}")
          case v @ Decl.Val(_, _, retType: Type.Name) if retType.value != typeAlias.name.value =>
            abort(
              s"Abstract val needs to have return type ${typeAlias.name}[...], otherwise, make it non-abstract: ${System
                .lineSeparator()} ${v.syntax}")
          case v: Defn.Var => abort(s"Found a var, which is not allowed: $v")
          case _           => ()
        }
      }
    }

    def getConcreteMembers(template: Template): List[Defn] = {
      template.stats.map { stats =>
        stats.collect {
          case d: Defn.Def => d
          case v: Defn.Val => v
        }.toList
      }.getOrElse(Nil)
    }

    def getAbstractMembers(template: Template): List[Decl] = {
      val typeAliasName = typeAlias.name.value
      template.stats.map { stats =>
        stats.collect {
          case m @ Decl.Def(_, _, _, _, Type.Apply(retName: Type.Select, _))
            if retName.name.value == typeAliasName =>
            m
          case v @ Decl.Val(_, _, Type.Apply(retName: Type.Select, _))
            if retName.name.value == typeAliasName =>
            v

          case m @ Decl.Def(_, _, _, _, Type.Apply(retName: Type.Name, _))
            if retName.value == typeAliasName =>
            m
          case v @ Decl.Val(_, _, Type.Apply(retName: Type.Name, _))
            if retName.value == typeAliasName =>
            v
        }.toList
      }.getOrElse(Nil)
    }

    def generateLiftedOps(abstractMembers: List[Decl]): (List[Defn.Def], List[Defn]) = {
      abstractMembers.collect {
        case Decl.Def(_, name, tparams, paramss, rt @ Type.Apply(_, innerType)) =>
          val op = {
            val args   = paramssToArgsFlatten(paramss)
            val rhs    = q"Free.liftF(I.inj(${adt(sealedFreeTrait, name)}(..$args)))"
            val params = (if (paramss.isEmpty) Seq.empty else paramss) :+ implicitInject
            q"def $name[..${F +: tparams}](...$params): Free[F, ..$innerType] = $rhs"
          }
          val opRef = {
            val args       = paramssToArgs(paramss)
            val typeParams = t"${sealedFreeTrait.name}" +: tParamsToTNames(tparams)
            val rhs        = methodCallFmt(q"injectOps.$name[..$typeParams]", args)
            if (tparams.nonEmpty)
              q"def $name[..$tparams](...$paramss): $rt = $rhs"
            else
              q"def $name(...$paramss): $rt = $rhs"
          }
          (op, opRef)
        case Decl.Val(_, Seq(nameTerm), rt @ Type.Apply(_, innerType)) =>
          val name = nameTerm.name
          val op = {
            val rhs = q"Free.liftF(I.inj(${adt(sealedFreeTrait, name)}))"
            q"def $name[$F](..$implicitInject): Free[F, ..$innerType] = $rhs"
          }
          val opRef = q"val $nameTerm: $rt = injectOps.$name[${sealedFreeTrait.name}]"
          (op, opRef)
      }.unzip
    }

    def generateNewConcreteMembers(
        concreteMembers: List[Defn],
        liftedOps: List[Defn.Def]): (List[Defn.Def], List[Defn], List[Defn], List[Defn]) = {
      val concretValOrDef = concreteMembers.collect {
        case d: Defn.Def => d
        case v: Defn.Val => v
      }
      val (ops, nonOps) = concretValOrDef.partition {
        // TODO confirm if required for IntelliJ
        case Defn.Def(_, _, _, _, Some(Type.Apply(outerType: Type.Select, _)), _) =>
          outerType.name.value == typeAlias.name.value
        case Defn.Val(_, _, Some(Type.Apply(outerType: Type.Select, _)), _) =>
          outerType.name.value == typeAlias.name.value

        case Defn.Def(_, _, _, _, Some(Type.Apply(outerType: Type.Name, _)), _) =>
          outerType.value == typeAlias.name.value
        case Defn.Val(_, _, Some(Type.Apply(outerType: Type.Name, _)), _) =>
          outerType.value == typeAlias.name.value
        case _ => false
      }

      // append type param F to all call to methods that will be in injectOps Obj (to satisfy F[_]).
      def addFTransformer(term: Term): Term = {
        val injectOps = liftedOps ++ ops

        def injectOpsContainsName(name: Term.Name): Boolean = {
          injectOps.exists {
            case Defn.Def(_, opName, _, _, _, _) => opName.value == name.value
            case Defn.Val(_, pats, _, _) =>
              pats.exists {
                case p: Pat.Var.Term => p.name.value == name.value
                case _               => false
              }
          }
        }

        val t = term.transform {
          case q"${name: Term.Name}[..$tp].$methName(..$args)" if injectOpsContainsName(name) => {
            q"$name[F, ..$tp].$methName(..$args)"
          }
          case q"${name: Term.Name}[..$tp](..$args)" if injectOpsContainsName(name) =>
            q"$name[F, ..$tp](..$args)"
        }
        t match {
          case term: Term => term
          case _          => abort("Error in Freast. Please file an issue")
        }
      }

      /*
        defs that contain the real implementation
        resist the temptation to remove the Some match despite the red underlines.
       */
      val injectOps = ops.collect {
        case q"..$mods def $name[..$tp](...$paramss): ${Some(Type.Apply(_, innerType))} = $rhs" =>
          q"..$mods def $name[$F, ..$tp](...${paramss :+ implicitInject}): Free[F, ..$innerType] = ${addFTransformer(rhs)}"
        case q"..$mods val ${pat: Pat.Var.Term}: ${Some(Type.Apply(_, innerType))} = $rhs" =>
          q"..$mods def ${pat.name}[$F](..$implicitInject): Free[F, ..$innerType] = ${addFTransformer(rhs)}"
      }

      // vals and defs that call to defs in injectOps
      val opsRef = ops.collect {
        case q"..$mods def $name[..$tparams](...$paramss): $rt = $_" => {
          val typeParams = t"${sealedFreeTrait.name}" +: tParamsToTNames(tparams)
          val args       = paramssToArgs(paramss)
          val rhs        = methodCallFmt(q"injectOps.$name[..$typeParams]", args)
          if (tparams.nonEmpty)
            q"..$mods def $name[..$tparams](...$paramss): $rt = $rhs"
          else
            q"..$mods def $name(...$paramss): $rt = $rhs"
        }
        case q"..$mods val ${pat: Pat.Var.Term}: $rt = $_" => {
          val name = pat.name
          q"..$mods val $pat: $rt = injectOps.$name[${sealedFreeTrait.name}]"
        }
      }

      // vals and defs that call other vals and defs in injectOps
      val nonOpsRef = nonOps.collect {
        case q"..$mods def $name[..$tparams](...$paramss): $rt = $_" => {
          val tNames = tParamsToTNames(tparams)
          val args   = paramssToArgs(paramss)
          val injectOps = if (tNames.nonEmpty) q"injectOps.$name[..$tNames]" else q"injectOps.$name"
          val rhs    = methodCallFmt(injectOps, args)
          if (tparams.nonEmpty)
            q"..$mods def $name[..$tparams](...$paramss): $rt = $rhs"
          else
            q"..$mods def $name(...$paramss): $rt = $rhs"
        }
        case q"..$mods val ${pats @ (pat: Pat.Var.Term)}: $rt = $_" => {
          q"..$mods val $pats: $rt = injectOps.${pat.name}"
        }
      }
      (injectOps, opsRef, nonOps, nonOpsRef)
    }

    def generateInjectClassAndObject(liftedOps: List[Defn.Def],
                                     concreteOps: List[Defn.Def],
                                     concreteNonOpsRef: List[Defn]): (Class, Object) = {
      val opsRef = (liftedOps ++ concreteOps).collect {
        case q"..$_ def $tname[..$tparams](...$paramss): $tpt = $_" =>
          val tNames = tParamsToTNames(tparams)
          val args   = paramssToArgs(paramss)
          val rhs    = methodCallFmt(q"injectOps.$tname[..$tNames]", args)
          // tail to remove the F[_] from tparams; dropRight(1) to remove implicit param
          if (tparams.tail.nonEmpty)
            q"def $tname[..${tparams.tail}](...${paramss.dropRight(1)}): $tpt = $rhs"
          else
            q"def $tname(...${paramss.dropRight(1)}): $tpt = $rhs"
      }
      val klass  = q"""
          class Injects[F[_]](implicit I: Inject[${sealedFreeTrait.name}, F]) {
            ..$opsRef
            ..$concreteNonOpsRef
          }"""
      val objekt = q"""
          object Injects {
            implicit def injectOps[F[_]](implicit I: Inject[${sealedFreeTrait.name}, F]): Injects[F] = new Injects[F]
          }
         """
      (klass, objekt)
    }

    def generateObjectContainingCaseClasses(abstractMembers: List[Decl]): Object = {
      val caseClasses = abstractMembers.collect {
        case q"..$mods def $name[..$tparams](...$paramss): ${Type.Apply(_, returnType)}" => {
          q"case class ${Type.Name(name.value.capitalize)}[..$tparams](..${paramss.flatten}) extends ${Ctor.Ref
            .Name(sealedFreeTrait.name.value)}[..$returnType]"
        }
        case q"..$mods val ${p: Pat.Var.Term}: ${Type.Apply(_, returnType)}" => {
          q"case object ${Term.Name(p.name.value.capitalize)} extends ${Ctor.Ref.Name(
            sealedFreeTrait.name.value)}[..$returnType]"
        }
      }
      q"""
        object ${Term.Name(sealedFreeTrait.name.value)} {
          ..$caseClasses
        }
      """
    }

    def generateInterpreterTrait(abstractMembers: List[Decl]): Trait = {
      // Replace the outermost type of the return type with a container type M
      val methodsToBeImpl = abstractMembers.collect {
        case q"..$mods def $name[..$tparams](...$paramss): ${rt: Type.Apply}" => {
          if (tparams.nonEmpty)
            q"..$mods def $name[..$tparams](...$paramss): ${rt.copy(tpe = t"M")}"
          else
            q"..$mods def $name(...$paramss): ${rt.copy(tpe = t"M")}"
        }
        case q"..$mods val ${p: Pat.Var.Term}: ${rt: Type.Apply}" =>
          q"..$mods def ${p.name}: ${rt.copy(tpe = t"M")}"
      }

      val cases = abstractMembers.collect {
        case q"..$_ def $name[..$_](...$paramss): $_" =>
          val binds = paramss.flatMap(_.collect {
            case t => p"${Pat.Var.Term(Term.Name(t.name.value))}"
          })
          val args      = paramssToArgs(paramss)
          val rhs: Term = if (args.isEmpty) name else q"$name(...$args)"
          p"case ${adt(sealedFreeTrait, name)}(..$binds) => $rhs"
        case q"..$_ val ${p: Pat.Var.Term}: $_" => {
          val pat: Pat =
            p"${Term.Name(sealedFreeTrait.name.value)}.${Term.Name(p.name.toString.capitalize)}"
          p"case $pat => ${p.name}"
        }
      }

      q"""
       trait Interp[M[_]] {
         import ops._
         val interpreter = new (${sealedFreeTrait.name} ~> M) {
           def apply[A](fa: ${sealedFreeTrait.name}[A]): M[A] = fa match {
             ..case $cases
           }
         }
         def run[A](op: ${typeAlias.name}[A])(implicit m: Monad[M]): M[A] = op.foldMap(interpreter)
         ..$methodsToBeImpl
       }
    """
    }

    // Helper methods

    private def adt(sealedTrait: Defn.Trait, name: Term.Name): Term.Ref =
      q"${Term.Name(sealedTrait.name.value)}.${Term.Name(name.toString.capitalize)}"

    private type Paramss = Seq[Seq[Term.Param]]
    private def paramssToArgs(paramss: Paramss): Seq[Seq[Term.Arg]] = {
      paramss.filter(_.nonEmpty).map(_.map(p => Term.Name(p.name.value)))
    }

    // ex: convert (a: A)(b: B) to (a, b)
    private def paramssToArgsFlatten(paramss: Paramss): Seq[Term.Arg] = {
      for {
        ps <- paramss
        p  <- ps
      } yield Term.Name(p.name.value)
    }

    private def methodCallFmt(method: Term, args: Seq[Seq[Term.Arg]]): Term =
      if (args.flatten.isEmpty) method else q"$method(...$args)"

    private def tParamsToTNames(tparams: Seq[Type.Param]): Seq[Type.Name] = {
      tparams.map(t => Type.Name(t.name.value))
    }

  }

}
