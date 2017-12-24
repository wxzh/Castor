package examples

import scala.meta._

import scala.collection.immutable.Seq

/** TODO: support GADT, e.g.
  * trait List[T] {
  *   def Nil: List[T]
  *   def Cons: (T, List[T]) => List[T]
  * }
  */

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
class visitor extends scala.annotation.StaticAnnotation

class vicase extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val DEBUG = true
    if (DEBUG) println(defn.structure)

    def uncapitalize(s: String) = if (s.length > 0) s(0).toLower + s.substring(1) else s

    def decl2ctr(adtName: String, tparams: Seq[Type.Param]): PartialFunction[Stat, Ctr] = {
      case Decl.Def(_, Term.Name(ctrName), Nil, Nil, tpe) =>
        val tparamNames = tparams.map{ _.name.value }
        tpe match {
          case Type.Function(ts, t) => Ctr(adtName, ctrName, tparamNames, ts.map(toType(_)), t)
          case _ => Ctr(adtName, ctrName, tparamNames,Seq(), tpe)
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
        //TODO: generate objects for traits that annotated as @visitor
        val companion = q"object ${Term.Name(t.name.value)} extends ${Template(Nil,Seq(Term.Apply(Ctor.Ref.Name(t.name.value), Nil)),Term.Param(Nil, Name.Anonymous(), None, None), None)}"
        println((newT,companion))
        Term.Block(Seq(newT, companion))
      case None => abort("Empty top-level trait")
    }

    def genImpl(defn: Stat) = defn match {
      case Defn.Trait(List(mod"@adt"), tname@Type.Name(name), tparams, _, Template(Nil, parents, _, body)) =>
        val visTrait = Type.Name(name + "Visitor")
        val defaultTrait = Type.Name(name + "Default")
        val visType = Type.Name(name + "V")
        val outType = Type.Name("O" + name)
        val outTypeApp = Type.Apply(outType, tparams.map{p=>Type.Name(p.name.value)})
        val adtType = Type.Name(name)
        val adtTypeApp = Type.Apply(adtType, tparams.map{p=>Type.Name(p.name.value)})
        val typeDecl = q"type $visType <: $visTrait"
        val visParent = Ctor.Ref.Name(name + "Visitor")

        val ctrs = body.map(stats => stats.map(decl2ctr(name, tparams)(_))).getOrElse(Nil)
        val adtDecl =
          q"""abstract class $adtType[..$tparams] {
                def accept(v: $visType): ${
                  if (tparams.isEmpty) Type.Select(Term.Name("v"), outType)
                  else Type.Apply(Type.Select(Term.Name("v"), outType), tparams.map(p => Type.Name(p.name.value)))}
              }"""
        val parents2 = parents.collect {
          case Term.Apply(Ctor.Ref.Select(t, Ctor.Ref.Name(name)), u) =>
            Term.Apply(Ctor.Ref.Select(t, visParent), u)
        }
        // visitor interface
        val outTypeDecl = Decl.Type(Nil, Type.Name("O"+name), tparams, Type.Bounds(None, None))
        val apply = q"def apply[..$tparams](x: $adtTypeApp) = x.accept(this)"
        val template = Template(Nil, parents2, Term.Param(Nil, Name.Anonymous(), Some(visType), None),
          Some((if (parents.isEmpty) Seq(outTypeDecl, apply) else Seq()) ++ ctrs.map(_.genVisit)))
        val visitorDecl = q"trait $visTrait extends $template"

        // default visitor
        val otherwise = q"def otherwise[..$tparams]: ${Type.Function(Seq(adtTypeApp),outTypeApp)}"
        val caseDecls = ctrs.map(_.genCase)
        val parents3 = parents.collect {
          case Term.Apply(Ctor.Ref.Select(t, Ctor.Ref.Name(name)), u) =>
            Term.Apply(Ctor.Ref.Select(t, Ctor.Ref.Name(name+"Default")), u)
        }
        val template3 = Template(Nil, Term.Apply(visParent, Nil) +: parents3, Term.Param(Nil, Name.Anonymous(), Some(visType), None),
          Some((if (parents.isEmpty) Seq(otherwise) else Seq()) ++ ctrs.map(_.genOtherwise)))
        val defaultDecl = q"trait $defaultTrait extends $template3"
        (if (parents.isEmpty) Seq(adtDecl) else Seq()) ++ Seq(typeDecl, visitorDecl, defaultDecl) ++ caseDecls

      case trt@Defn.Trait(List(mod"@visitor"), tname@Type.Name(name), _, _, _) =>
        Seq(trt)//, q"val ${Pat.Var.Term(Term.Name(uncapitalize(name)))}: $tname")

      case stat => Seq(stat)
    }

    defn match {
      case t: Defn.Trait => gen(t)
      case Term.Block(Seq(t: Defn.Trait, o: Defn.Object)) => Term.Block(Seq(gen(t),o))
      case _ => abort("Not a trait")
    }
  }
}

case class Ctr(adtName: String, ctrName: String, tparams: Seq[String], ts: Seq[Type], t: Type) {
  val ot = if (tparams.isEmpty) Type.Name("O" + adtName) else Type.Apply(Type.Name("O" + adtName), t.asInstanceOf[Type.Apply].args)
  val xs = for (i <- 1 to ts.length) yield Term.Name("x"+i)
  val params = Seq((xs,ts).zipped.map { (x,t) => param"$x: $t" })
  val visitMethod = Term.Name(ctrName(0).toLower + ctrName.substring(1))

  val allTvars = (t +: ts).flatMap { t =>
    t match {
      case Type.Apply(Type.Name(name), ts) if name == adtName => ts.collect { case Type.Name(n) => n }
      case _ => Seq()
    }
  }
  val usedTvars = tparams.intersect(allTvars).map{name => Type.Param(Nil, Type.Name(name), Nil, Type.Bounds(None, None), Nil, Nil)}

  // is generic when tparams are used in ts, t
  def genVisit =
    q"def $visitMethod[..$usedTvars]: ${if (ts.isEmpty) ot else Type.Function(ts,ot)}"

  def genCase = {
    val accept = {
      val selectCtr = Term.Select(Term.Name("v"), visitMethod)
      q"""def accept(v: ${Type.Name(adtName + "V")}) =
        ${ if (ts.isEmpty) selectCtr else Term.Apply(selectCtr, xs)}"""
    }
    // TODO generic extends generic trait
    val adt = if (tparams.isEmpty) Ctor.Ref.Name(adtName) else Term.Apply(Term.ApplyType(Ctor.Ref.Name(adtName), t.asInstanceOf[Type.Apply].args), Nil)
    if (ts.isEmpty && usedTvars.isEmpty) q"case object ${Term.Name(ctrName)} extends $adt {..${Seq(accept)}}"
    else q"""case class ${Type.Name(ctrName)}[..$usedTvars] (...$params) extends $adt {..${Seq(accept)}}"""
  }

  // reconstructing the object
  def genOtherwise = {
    val body =
      if (xs.isEmpty) Term.Apply(Term.Name("otherwise"), if (usedTvars.isEmpty) Seq(Term.Name(ctrName)) else Seq(Term.Apply(Term.Name(ctrName), Seq())))
      else Term.Function(xs.map{x => param"$x"}, Term.Apply(Term.Name("otherwise"), Seq(Term.Apply(Term.Name(ctrName), xs))))
    q"def $visitMethod[..$usedTvars] = $body"
  }

  //tparams
//Decl.Type(Nil, Type.Name("O"+name), Seq(Type.Param(Nil, Name.Anonymous(), Nil, Type.Bounds(None, None), Nil, Nil)), Type.Bounds(None, None))
//  def typeName(t: Type) = t match {
//    case Type.Name(tname) if tname == name => tname
//    case Type.Apply(tname, Seq()) if tname == name =>
//    case _ => abort("Invalid constructor")
//  }
}

