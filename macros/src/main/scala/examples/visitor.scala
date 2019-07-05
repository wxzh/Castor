package examples

import scala.meta._
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.collection.mutable._

// Defn.Class(Seq(Mod.Annot(Term.Apply(Ctor.Ref.Name("ctr"), Seq(Term.Name("Exp"))))),
//TODO: otherwise -> adtName for language composition
//TODO: default for hierarchical datatypes

class adt extends scala.annotation.StaticAnnotation
class visit[T](t: T*) extends scala.annotation.StaticAnnotation
class default[T](t: T) extends scala.annotation.StaticAnnotation
class adts[T](adts: T*) extends scala.annotation.StaticAnnotation
class ops[T](ops: T*) extends scala.annotation.StaticAnnotation
class skip extends scala.annotation.StaticAnnotation
//class ctr[T](t: T) extends scala.annotation.StaticAnnotation
class vctr[T](t: T) extends scala.annotation.StaticAnnotation
class ctrs[T](adts: T*) extends scala.annotation.StaticAnnotation


class family extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val DEBUG = true
    if (DEBUG) println(defn.structure)
    //    val variants = Map[String,MutableList[String]]()

    def uncapitalize(s: String) = if (s.length > 0) s(0).toLower + s.substring(1) else s

    def isCtr(stat: Stat): Boolean = stat match {
      case _: Defn.Class => true
      case _: Defn.Trait => true
      case _: Defn.Object => true
      case _ => false
    }
    def decl2ctr(adtName: String): PartialFunction[Stat,Constructor] = {
      // def C: Ts => T
      //      case Decl.Def(_, Term.Name(ctrName), tparams, Nil, tpe) =>
      //        tpe match {
      //          case Type.Function(ts, t) =>
      //            val adt = t match {
      //              case _: Type.Name => Ctor.Ref.Name(adtName)
      //              case Type.Apply(_,args) => Term.Apply(Term.ApplyType(Ctor.Ref.Name(adtName), args), Nil)
      //            }
      //            val xs = for (i <- 1 to ts.length) yield Term.Name("x"+i)
      //            val params = Seq((xs,ts).zipped.map { (x,t) => param"$x: $t" })
      //            val s = q"case class ${Type.Name(ctrName)}[..$tparams] (...$params) extends $adt {}"
      //            Constructor(adtName, ctrName, tparams, ts.map(toType(_)), t, s)
      //          case _ =>
      //            val adt = tpe match {
      //              case _: Type.Name => Ctor.Ref.Name(adtName)
      //              case Type.Apply(_,args) => Term.Apply(Term.ApplyType(Ctor.Ref.Name(adtName), args), Nil)
      //            }
      //            val s = q"case object ${Term.Name(ctrName)} extends $adt {}"
      //            Constructor(adtName, ctrName, tparams, Seq(), tpe, s)
      //        }

      case self@Defn.Class(_, Type.Name(ctrName), tvars, _, template) =>
        Constructor(adtName,ctrName,tvars,template,self)
      case self@Defn.Trait(_, t@Type.Name(ctrName), tvars, _, template) =>
        Constructor(adtName,ctrName,tvars,template,self)
      case self@Defn.Object(_, t@Term.Name(ctrName), template) =>
        Constructor(adtName,ctrName,Nil,template,self)
    }


    def toType(arg: Type.Arg): Type = arg match {
      case Type.Arg.Repeated(tpe) => tpe
      case Type.Arg.ByName(tpe) => tpe
      case tpe: Type => tpe
    }

    def typeName(name: String) = Type.Name(name + "V")

    def gen(t: Defn.Trait) = t.templ.stats match {
      case Some(stats) =>
        val newT = t.copy(
          mods = Seq(),
          templ = t.templ.copy(
            stats = t.templ.stats.map(stats => stats.flatMap(genImpl(_,stats))))
        )
        val binds = t.mods.collect {
          case Mod.Annot(Term.Apply(Ctor.Ref.Name("adts"), names)) => names.collect{ case Term.Name(n) => n }
        }.flatten ++ stats.collect {
          case Defn.Trait(List(mod"@adt"), Type.Name(name), _, _, _) => name
        } map { name =>
          q"type ${typeName(name)} = ${Type.Name(name + "Visit")}"
        }
        val ctrBinds = t.mods.collect {
          case Mod.Annot(Term.Apply(Ctor.Ref.Name("ctrs"), names)) => names.collect{ case Term.Name(n) => n }
        }.flatten ++ stats.collect {
          case Defn.Trait(List(Mod.Annot(Term.Apply(Ctor.Ref.Name("vctr"), Seq(_)))), Type.Name(name), _, _, _) => name
        } map { name =>
          q"type ${typeName(name)} = ${Type.Name(name)}"
        }

        val objs = t.mods.collect {
          case Mod.Annot(Term.Apply(Ctor.Ref.Name("ops"), names)) => names.collect{ case Term.Name(n) => n }
        }.flatten ++ stats.collect {
          case Defn.Trait(Seq(Mod.Annot(Term.Apply(Ctor.Ref.Name(op), _))), Type.Name(name), _, _, _) if op == "visit" || op == "default" => name
        } map { name =>
          q"object ${Term.Name(uncapitalize(name))} extends ${Template(Nil,Seq(Term.Apply(Ctor.Ref.Name(name), Nil)),Term.Param(Nil, Name.Anonymous(), None, None), None)}"
        }
        val companion = q"object ${Term.Name(t.name.value)} extends ${Template(Nil,Seq(Term.Apply(Ctor.Ref.Name(t.name.value), Nil)),Term.Param(Nil, Name.Anonymous(), None, None), Some(binds ++ ctrBinds ++ objs))}"
        println((newT,companion))
        Term.Block(Seq(newT, companion))
      case None => abort("Empty top-level trait")
    }

    def genImpl(defn: Stat, defns: Seq[Stat]) = defn match {

      case trt@Defn.Trait(List(mod"@adt"), tname@Type.Name(name), tparams, _, Template(Nil, parents, _, body)) =>
        val visTrait = Type.Name(name + "Visit")
        val defaultTrait = Type.Name(name + "Default")
        val visType = typeName(name)
        val outType = Type.Name("O" + name)
        val tvars = tparams.map{p=>Type.Name(p.name.value)}
        val outTypeApp = if (tvars.isEmpty) outType else Type.Apply(outType, tvars)
        val adtType = Type.Name(name)
        val adtTypeApp = if (tvars.isEmpty) adtType else Type.Apply(adtType, tvars)
        val typeDecl = q"type $visType <: $visTrait"
        val visParent = Ctor.Ref.Name(name + "Visit")

        val ctrs = body.map(stats => stats.collect(decl2ctr(name))).getOrElse(Nil)

        val accept = q"""def accept(v: $visType): ${
          if (tparams.isEmpty) Type.Select(Term.Name("v"), outType)
          else Type.Apply(Type.Select(Term.Name("v"), outType), tparams.map(p => Type.Name(p.name.value)))}
              """
        val parents2 = parents.collect {
          case Term.Apply(Ctor.Ref.Select(t, Ctor.Ref.Name(name)), u) =>
            Term.Apply(Ctor.Ref.Select(t, visParent), u)
          case Term.Apply(Term.ApplyType(Ctor.Ref.Select(t, Ctor.Ref.Name(name)), _), u) =>
            Term.Apply(Ctor.Ref.Select(t, visParent), u)
          case p => abort(p.structure)
            Term.Apply(Term.ApplyType(Ctor.Ref.Select(Term.Super(Name.Anonymous(), Name.Anonymous()), Ctor.Ref.Name("Tm")), Seq(Type.Name("A"))), Nil)
        }
        // visitor interface
        val outTypeDecl = Decl.Type(Nil, Type.Name("O"+name), tparams, Type.Bounds(None, None))
        val apply = q"def apply[..$tparams](x: $adtTypeApp) = x.accept(this)"
        val template = Template(Nil, parents2, Term.Param(Nil, Term.Name("rec"), Some(visType), None),
          Some((if (parents.isEmpty) Seq(outTypeDecl, apply) else Seq()) ++ ctrs.map(_.genVisit).flatten))
        val visitorDecl = q"trait $visTrait extends $template"

        // default visitor
        val default = Term.Name(uncapitalize(name))
        val otherwise = q"def $default[..$tparams]: ${Type.Function(Seq(adtTypeApp),outTypeApp)}"
        val parents3 = parents.collect {
          case Term.Apply(Ctor.Ref.Select(t, Ctor.Ref.Name(name)), u) =>
            Term.Apply(Ctor.Ref.Select(t, Ctor.Ref.Name(name+"Default")), u)
          case Term.Apply(Term.ApplyType(Ctor.Ref.Select(t, Ctor.Ref.Name(name)), _), u) =>
            Term.Apply(Ctor.Ref.Select(t, Ctor.Ref.Name(name+"Default")), u)
        }
        val template3 = Template(Nil, Term.Apply(visParent, Nil) +: parents3, Term.Param(Nil, Name.Anonymous(), Some(visType), None),
          Some((if (parents.isEmpty) Seq(otherwise) else Seq()) ++ ctrs.map(_.genDefault)))
        val defaultDecl = q"trait $defaultTrait extends $template3"

        val newTrt = trt.copy(
          mods = Seq(), // remove annotation
          templ = trt.templ.copy(
            //            parents = if (trt.templ.parents.isEmpty) Seq(Ctor.Ref.Name(name)) else trt.templ.parents,
            stats = trt.templ.stats.map(stats => accept +: stats.filterNot(isCtr(_))).orElse(Some(Seq(accept)))
          )
        )
        (if (parents.isEmpty) Seq(newTrt) else Seq()) ++ Seq(typeDecl,visitorDecl,defaultDecl) ++ ctrs.map(_.genAccept) //++ skipped

      case trt@Defn.Trait(Seq(Mod.Annot(Term.Apply(Ctor.Ref.Name(visitType), adts))), tname@Type.Name(name), _, _, Template(_, parents, _, body)) =>
        val adtNames = adts.collect{case Term.Name(name) => name}
        val newTrt = trt.copy(
          mods = Seq(),
          templ = trt.templ.copy(
            parents = adtNames.map{n => Ctor.Ref.Name(n + visitType.capitalize)} ++ trt.templ.parents,
            self = Term.Param(Nil, Term.Name("rec"),
              adtNames.map{n => typeName(n).asInstanceOf[Type] }.reduceLeftOption((x,y) => Type.With(x,y)), None)
          )
        )
        if (parents.collectFirst{case p@Term.Apply(Ctor.Ref.Select(_, Ctor.Ref.Name(n)), _) if n == name => p}.isEmpty)
          Seq(newTrt, q"val ${Pat.Var.Term(Term.Name(uncapitalize(name)))}: $tname")
        else
          Seq(newTrt)

      case stat => Seq(stat)
    }

    case class Constructor(adtName: String, ctrName: String, tparams: Seq[Type.Param], template: Template, stat: Stat) {
      val (defaultName,targs) = template.parents match {
        case Nil =>
          ("",Nil)
        case List(Term.Apply(Ctor.Ref.Name(name),_)) => // extends C
          (name,Nil)
        case List(Term.Apply(Term.ApplyType(Ctor.Ref.Name(name),ts),Nil)) => // extends D[...]
          (name,ts)
        case List(Term.Apply(Ctor.Ref.Name(name),Nil), Term.Apply(Term.ApplyType(Ctor.Ref.Name(_),ts),Nil)) => // extends C2 with D[...]
          (name,ts)
        case x =>
          abort("Unrecognized parents"+x.toString)
      }
      val ot = targs match {
        case Nil => Type.Name("O" + adtName)
        case _  => Type.Apply(Type.Name("O" + adtName),targs)
      }
      val visitMethod = Term.Name(uncapitalize(ctrName))
      val default = Term.Name(uncapitalize(if (defaultName == "") adtName else defaultName))
      val self =
        if (tparams.isEmpty) Type.Name(ctrName)
        else Type.Apply(Type.Name(ctrName),tparams.map{tparam => Type.Name(tparam.name.toString)})

      def genVisit =
        stat match {
          case _: Defn.Trait => None
          case _: Defn.Object => Some(q"def $visitMethod[..$tparams]: $ot")
          case _: Defn.Class => Some(q"def $visitMethod[..$tparams]: ${Type.Function(Seq(self),ot)}")
        }

      def genDefault =
        stat match {
          case _: Defn.Object => q"def $visitMethod[..$tparams] = $default(${Term.Name(ctrName)})"
          case _ => q"def $visitMethod[..$tparams] = (x: $self) => $default(x)"
        }

      def genAccept = {
        val adtV = typeName(adtName)
        val visitCall =
          if (stat.isInstanceOf[Defn.Object]) q"v.${visitMethod}"
          else q"v.${visitMethod}(this)"
        val accept = q"override def accept(v: $adtV) = $visitCall"

        val newTempl = template.copy(
          parents = if (template.parents.isEmpty) Seq(Ctor.Ref.Name(adtName)) else template.parents,
          stats =
            if (stat.isInstanceOf[Defn.Trait]) template.stats
            else template.stats.map(accept +: _).orElse(Some(Seq(accept)))
        )
        stat match {
          case s: Defn.Trait => s.copy(templ=newTempl)
          case s: Defn.Object => s.copy(templ=newTempl)
          case s: Defn.Class => s.copy(templ=newTempl)
        }
      }
    }

    defn match {
      case t: Defn.Trait => gen(t)
      case Term.Block(Seq(t: Defn.Trait, o: Defn.Object)) => Term.Block(Seq(gen(t),o))
      case _ => abort("Not a trait")
    }
  }

}

