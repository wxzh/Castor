package examples

import scala.meta._

import scala.collection.immutable.Seq

/**
  * Naming Convention
  * ---
  * @family trait Term {
  *   @adt trait Tm
  * }
  * ~>
  * trait Term {
  *   type TmV <: TmVisit
  *   abstract class Tm {
  *     def accept(v: TmVisit): v.OTm
  *   }
  *   trait TmVisit {_: TmV =>
  *     type OTm
  *     def apply(x: Tm): OTm = x.accept(this)
  *   }
  * }
  *
  * trait Nat extends Term {
  *   @adt trait Tm extends super.Tm {
  *     def TmZero: Tm
  *     def TmSucc: Tm => Tm
  *   }
  *
  *   case object TmZero extends Tm
  *   case class TmSucc(x: Tm) extends Tm
  *   trait TmVisit extends super.TmVisit {
  *     def tmZero: OTm
  *     def tmSucc: Tm => OTm
  *   }
  * }
  *
 */
class adt extends scala.annotation.StaticAnnotation
class visit[T](t: T) extends scala.annotation.StaticAnnotation
class default[T](t: T) extends scala.annotation.StaticAnnotation
class adts[T](adts: T*) extends scala.annotation.StaticAnnotation
class ops[T](ops: T*) extends scala.annotation.StaticAnnotation


class family extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val DEBUG = false
    if (DEBUG) println(defn.structure)

    def uncapitalize(s: String) = if (s.length > 0) s(0).toLower + s.substring(1) else s

    def decl2ctr(adtName: String): PartialFunction[Stat, Ctr] = {
      case Decl.Def(_, Term.Name(ctrName), tparams, Nil, tpe) =>
        tpe match {
          case Type.Function(ts, t) => Ctr(adtName, ctrName, tparams, ts.map(toType(_)), t)
          case _ => Ctr(adtName, ctrName, tparams, Seq(), tpe)
        }
      case d => abort("Unrecognized Def:" + d.structure)
    }

    def toType(arg: Type.Arg): Type = arg match {
      case Type.Arg.Repeated(tpe) => tpe
      case Type.Arg.ByName(tpe) => tpe
      case tpe: Type => tpe
    }

    def gen(t: Defn.Trait) = t.templ.stats match {
      case Some(stats) =>
        val newT = t.copy(
          mods = Seq(),
          templ = t.templ.copy(
            stats = t.templ.stats.map(stats => stats.flatMap(genImpl(_))))
          )
        val binds = t.mods.collect {
          case Mod.Annot(Term.Apply(Ctor.Ref.Name("adts"), names)) => names.collect{ case Term.Name(n) => n }
        }.flatten ++ stats.collect {
            case Defn.Trait(List(mod"@adt"), Type.Name(name), _, _, _) => name
        } map { name =>
          q"type ${Type.Name(name + "V")} = ${Type.Name(name + "Visit")}"
        }

        val objs = t.mods.collect {
          case Mod.Annot(Term.Apply(Ctor.Ref.Name("ops"), names)) => names.collect{ case Term.Name(n) => n }
        }.flatten ++ stats.collect {
          case Defn.Trait(Seq(Mod.Annot(Term.Apply(Ctor.Ref.Name(op), _))), Type.Name(name), _, _, _) if op == "visit" || op == "default" => name
        } map { name =>
          q"object ${Term.Name(uncapitalize(name))} extends ${Template(Nil,Seq(Term.Apply(Ctor.Ref.Name(name), Nil)),Term.Param(Nil, Name.Anonymous(), None, None), None)}"
        }
        val companion = q"object ${Term.Name(t.name.value)} extends ${Template(Nil,Seq(Term.Apply(Ctor.Ref.Name(t.name.value), Nil)),Term.Param(Nil, Name.Anonymous(), None, None), Some(binds ++ objs))}"
        println((newT,companion))
        Term.Block(Seq(newT, companion))
      case None => abort("Empty top-level trait")
    }


    def genImpl(defn: Stat) = defn match {
      case Defn.Trait(List(mod"@adt"), tname@Type.Name(name), tparams, _, Template(Nil, parents, _, body)) =>
        val visTrait = Type.Name(name + "Visit")
        val defaultTrait = Type.Name(name + "Default")
        val visType = Type.Name(name + "V")
        val outType = Type.Name("O" + name)
        val tvars = tparams.map{p=>Type.Name(p.name.value)}
        val outTypeApp = if (tvars.isEmpty) outType else Type.Apply(outType, tvars)
        val adtType = Type.Name(name)
        val adtTypeApp = if (tvars.isEmpty) adtType else Type.Apply(adtType, tvars)
        val typeDecl = q"type $visType <: $visTrait"
        val visParent = Ctor.Ref.Name(name + "Visit")

        val ctrs = body.map(stats => stats.map(decl2ctr(name)(_))).getOrElse(Nil)
        val adtDecl =
          q"""abstract class $adtType[..$tparams] {
                def accept(v: $visType): ${
                  if (tparams.isEmpty) Type.Select(Term.Name("v"), outType)
                  else Type.Apply(Type.Select(Term.Name("v"), outType), tparams.map(p => Type.Name(p.name.value)))}
              }"""
        val parents2 = parents.collect {
          case Term.Apply(Ctor.Ref.Select(t, Ctor.Ref.Name(name)), u) =>
            Term.Apply(Ctor.Ref.Select(t, visParent), u)
          case Term.Apply(Term.ApplyType(Ctor.Ref.Select(t, Ctor.Ref.Name(name)), _), u) =>
            Term.Apply(Ctor.Ref.Select(t, visParent), u)
//          case p => abort(p.structure)
//            Term.Apply(Term.ApplyType(Ctor.Ref.Select(Term.Super(Name.Anonymous(), Name.Anonymous()), Ctor.Ref.Name("Tm")), Seq(Type.Name("A"))), Nil)
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
          case Term.Apply(Term.ApplyType(Ctor.Ref.Select(t, Ctor.Ref.Name(name)), _), u) =>
            Term.Apply(Ctor.Ref.Select(t, Ctor.Ref.Name(name+"Default")), u)
        }
        val template3 = Template(Nil, Term.Apply(visParent, Nil) +: parents3, Term.Param(Nil, Name.Anonymous(), Some(visType), None),
          Some((if (parents.isEmpty) Seq(otherwise) else Seq()) ++ ctrs.map(_.genOtherwise)))
        val defaultDecl = q"trait $defaultTrait extends $template3"
        (if (parents.isEmpty) Seq(adtDecl) else Seq()) ++ Seq(typeDecl, visitorDecl, defaultDecl) ++ caseDecls

      case trt@Defn.Trait(Seq(Mod.Annot(Term.Apply(Ctor.Ref.Name(visitType), Seq(Term.Name(adtName))))), tname@Type.Name(name), _, _, Template(_, parents, _, body)) =>
        val newTrt = trt.copy(
          mods = Seq(),
          templ = trt.templ.copy(
            parents = Ctor.Ref.Name(adtName + visitType.capitalize) +: trt.templ.parents,
            self = Term.Param(Nil, Name.Anonymous(), Some(Type.Name(adtName + "V")), None)
          )
        )
        if (parents.collectFirst{case p@Term.Apply(Ctor.Ref.Select(_, Ctor.Ref.Name(n)), _) if n == name => p}.isEmpty)
          Seq(newTrt, q"val ${Pat.Var.Term(Term.Name(uncapitalize(name)))}: $tname")
        else
        Seq(newTrt)

      case stat => Seq(stat)
    }

    defn match {
      case t: Defn.Trait => gen(t)
      case Term.Block(Seq(t: Defn.Trait, o: Defn.Object)) => Term.Block(Seq(gen(t),o))
      case _ => abort("Not a trait")
    }
  }
}

case class Ctr(adtName: String, ctrName: String, tparams: Seq[Type.Param], ts: Seq[Type], t: Type) {
  val ot = t match {
    case _: Type.Name => Type.Name("O" + adtName)
    case Type.Apply(_,args) => Type.Apply(Type.Name("O" + adtName),args)
  }
  //if (tparams.isEmpty) Type.Name("O" + adtName) else Type.Apply(Type.Name("O" + adtName), t.asInstanceOf[Type.Apply].args)
//    t.asInstanceOf[Type.Apply].copy { name = "O" + adtName }
  val xs = for (i <- 1 to ts.length) yield Term.Name("x"+i)
  val params = Seq((xs,ts).zipped.map { (x,t) => param"$x: $t" })
  val visitMethod = Term.Name(ctrName(0).toLower + ctrName.substring(1))

  def genVisit =
    q"def $visitMethod[..$tparams]: ${if (ts.isEmpty) ot else Type.Function(ts,ot)}"

  def genCase = {
    val accept = {
      val selectCtr = Term.Select(Term.Name("v"), visitMethod)
      q"""def accept(v: ${Type.Name(adtName + "V")}) =
        ${ if (ts.isEmpty) selectCtr else Term.Apply(selectCtr, xs)}"""
    }
    val adt = t match {
      case _: Type.Name => Ctor.Ref.Name(adtName)
      case Type.Apply(_,args) => Term.Apply(Term.ApplyType(Ctor.Ref.Name(adtName), args), Nil)
    }

    if (ts.isEmpty && tparams.isEmpty) q"case object ${Term.Name(ctrName)} extends $adt {..${Seq(accept)}}"
    else q"""case class ${Type.Name(ctrName)}[..$tparams] (...$params) extends $adt {..${Seq(accept)}}"""
  }

  // reconstructing the object
  def genOtherwise = {
    val body =
      if (xs.isEmpty) Term.Apply(Term.Name("otherwise"), if (tparams.isEmpty) Seq(Term.Name(ctrName)) else Seq(Term.Apply(Term.Name(ctrName), Seq())))
      else Term.Function(xs.map{x => param"$x"}, Term.Apply(Term.Name("otherwise"), Seq(Term.Apply(Term.Name(ctrName), xs))))
    q"def $visitMethod[..$tparams] = $body"
  }
}

