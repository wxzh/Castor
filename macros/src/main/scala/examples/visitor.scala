package examples

import scala.meta._

import scala.collection.immutable.Seq

/**
  * Naming Convention
  * ---
  * trait Term {
  *   trait Tm
  * }
  * ~>
  * trait Term {
  *   type TmV <: TmVisitor
  *   abstract class Tm {
  *     def accept(v: TmVisitor): v.OTm
  *   }
  *   trait TmVisitor {_: TmV =>
  *     type OTm
  *     def apply(x: Tm): OTm = x.accept(this)
  *   }
  * }
  *
  * trait Nat extends Term {
  *   trait Tm extends super.Tm {
  *     def TmZero: Tm
  *     def TmSucc: Tm => Tm
  *   }
  *
  *   case object TmZero extends Tm
  *   case class TmSucc(x: Tm) extends Tm
  *   trait TmVisitor extends ... {
  *     def tmZero: Tm => T
  *   }
  * }
  *
 */
class adt extends scala.annotation.StaticAnnotation

class visitor extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val DEBUG = false
    if (DEBUG) println(defn.structure)

    def decl2ctr(name: String): PartialFunction[Stat, Ctr] = {
      case Decl.Def(_, Term.Name(name), Nil, Nil, tpe) =>
        tpe match {
          case Type.Function(ts, t: Type.Name) => Ctr(name, ts.map(toType(_)), t)
          case t: Type.Name => Ctr(name, Seq(), t)
        }
    }

    def toType(arg: Type.Arg): Type = arg match {
      case Type.Arg.Repeated(tpe) => tpe
      case Type.Arg.ByName(tpe) => tpe
      case tpe: Type => tpe
    }

    def gen(t: Defn.Trait) = t.templ.stats match {
      case Some(stats) =>
        val newT = t.copy(
          templ = t.templ.copy(
            stats = t.templ.stats.map(stats => stats.flatMap(genImpl(_))))
          )
        println(newT)
        q"$newT"
        case None => abort("..")
      }

    def genImpl(defn: Stat) = defn match {
      case Defn.Trait(List(mod"@adt"), tname@Type.Name(name), _, _, Template(Nil, parents, _, body)) =>
        val visTrait = Type.Name(name + "Visitor")
        val defaultTrait = Type.Name(name + "Default")
        val visType = Type.Name(name + "V")
        val outType = Type.Name("O" + name)
        val adtType = Type.Name(name)
        val typeDecl = q"type $visType <: $visTrait"
        val visParent = Ctor.Ref.Name(name + "Visitor")

        (parents, body) match {
          case (Nil, None) =>
            // data type declaration
            val adtDecl = q"abstract class $adtType { def accept(v: $visType): ${Type.Select(Term.Name("v"), outType)}}"
            val visitorDecl =
              q"""trait $visTrait {_: $visType =>
                            type ${Type.Name("O" + name)}
                            def apply(x: $adtType): $outType = x.accept(this)
                          }
                       """
            val otherwise = q"def otherwise: ${Type.Function(Seq(adtType),outType)}"
            val template = Template(Nil, Seq(Term.Apply(visParent, Nil)), Term.Param(Nil, Name.Anonymous(), Some(visType), None), Some(Seq(otherwise)))
            val defaultDecl = q"trait $defaultTrait extends $template"
            Seq(typeDecl, adtDecl, visitorDecl, defaultDecl)

          case (parents, Some(stats)) =>
            // concrete cases
            val parents2 = parents.collect {
              case Term.Apply(Ctor.Ref.Select(t, Ctor.Ref.Name(name)), u) =>
                Term.Apply(Ctor.Ref.Select(t, visParent), u)
            }
            val ctrs = stats.map(decl2ctr(name)(_))
            val template = Template(Nil, parents2, Term.Param(Nil, Name.Anonymous(), Some(visType), None), Some(ctrs.map(_.genVisit)))
            val visitorDecl = q"trait $visTrait extends $template"
            val caseDecls = ctrs.map(_.genCase)
            val parents3 = parents.collect {
              case Term.Apply(Ctor.Ref.Select(t, Ctor.Ref.Name(name)), u) =>
                Term.Apply(Ctor.Ref.Select(t, Ctor.Ref.Name(name+"Default")), u)
            }
            val template3 = Template(Nil, Term.Apply(visParent, Nil) +: parents3, Term.Param(Nil, Name.Anonymous(), Some(visType), None), Some(ctrs.map(_.genOtherwise)))
            val defaultDecl = q"trait $defaultTrait extends $template3"
            Seq(typeDecl, visitorDecl, defaultDecl) ++ caseDecls
          case (_, _) => Seq()
        }
      case stat => Seq(stat)
    }

    defn match {
      case t: Defn.Trait => gen(t)
      case Term.Block(Seq(t: Defn.Trait, o: Defn.Object)) => Term.Block(Seq(gen(t),o))
      case _ => abort("Not a trait")
    }
  }
}

case class Ctr(name: String, ts: Seq[Type], t: Type.Name) {
  val ot = Type.Name("O" + t.value)
  val xs = for (i <- 1 to ts.length) yield Term.Name("x"+i)
  val params = Seq((xs,ts).zipped.map { (x,t) => param"$x: $t" })
  val visitMethod = Term.Name(name(0).toLower + name.substring(1))
  def genVisit =
    q"def $visitMethod: ${if (ts.isEmpty) ot else Type.Function(ts,ot)}"

  def genCase = {
    val accept = {
      val selectCtr = Term.Select(Term.Name("v"), visitMethod)
      q"def accept(v: ${Type.Name(t.value + "V")}): ${Type.Select(Term.Name("v"), ot)} = ${ if (ts.isEmpty) selectCtr else Term.Apply(selectCtr, xs)}"
    }
    if (ts.isEmpty) q"case object ${Term.Name(name)} extends ${Ctor.Ref.Name(t.value)} {..${Seq(accept)}}"
    else q"case class ${Type.Name(name)} (...$params) extends ${Ctor.Ref.Name(t.value)} {..${Seq(accept)}}"
  }

  // reconstructing the object
  def genOtherwise = {
    val body =
      if (xs.isEmpty) Term.Apply(Term.Name("otherwise"), Seq(Term.Name(name)))
      else Term.Function(xs.map{x => param"$x"}, Term.Apply(Term.Name("otherwise"), Seq(Term.Apply(Term.Name(name), xs))))
    q"def $visitMethod = $body"
  }
}

