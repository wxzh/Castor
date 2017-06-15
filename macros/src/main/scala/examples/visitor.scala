package examples

import scala.meta._

import scala.collection.immutable.Seq

class visitor extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val DEBUG = true
    if (DEBUG) println(defn.structure)

    def toType(arg: Type.Arg): Type = arg match {
      case Type.Arg.Repeated(tpe) => tpe
      case Type.Arg.ByName(tpe) => tpe
      case tpe: Type => tpe
    }

    def decl2ctr(tnames: Seq[String]): PartialFunction[Stat, Ctr] = {
      case Decl.Def(mod,Term.Name(name),Nil,Nil,tpe) =>
        val old = mod match {
          case Nil => false
          case Seq(Mod.Override()) => true
        }
        tpe match {
          case Type.Function(ts,t: Type.Name) => Ctr(name, ts.map(toType(_)), t, old)
          case t: Type.Name => Ctr(name, Seq(), t, old)
        }
    }


    // TODO: generate toString() and equals() in AST hierarchy
    // TODO: @Obj automatically instantiate

    defn match {
        // assumption Some(stats): old methods should be explicitly overidden and hence the body is non-empty
      case Defn.Trait(_, tname @ Type.Name(name), _, _, Template(Nil,parents,_,Some(stats))) =>
        val in_tpes = stats.collect { case t@q"type $_" => t }
        val tnames = in_tpes map { case q"..$_ type $name" => name.value}
        val out_tpes = tnames.map { x => q"type ${Type.Name("O"+x)}" }
        val ctrs = stats.collect(decl2ctr(tnames))
        val (old_ctrs, new_ctrs) = ctrs.partition(_.old)
        val visit_decls = tnames.map{x =>
          q"def ${Term.Name("visit"+x)}: (..${Seq(Type.Name(x))}) => ${Type.Name("O"+x)}"
        }

        val super_facts =
          parents.map{case q"${name: Ctor.Name}()" => ctor"${Ctor.Ref.Name(name+".Fact")}"}
        val fact =
          q"trait ${Type.Name("Fact")} extends ..${super_facts} { ..${ in_tpes ++ new_ctrs.map{_.genLike} ++ new_ctrs.map{_.genField}} }"
        val fact_ref = ctor"${Ctor.Ref.Name(name+".Fact")}"
        val fact_ctor = ctor"${Ctor.Ref.Name("Fact")}"

        val tpe_refts = tnames.map{x => Defn.Type(Nil,Type.Name(x),Nil,Type.Name("C"+x))}
        val a_visitor = t"$tname {..$tpe_refts}"
        val a_visitor_ctor = ctor"${Ctor.Ref.Name(name)}"
        val c_visitor = q"trait ${Type.Name("C"+name)} extends ..${Seq(a_visitor_ctor, Ctor.Ref.Name("CFact"))} { ..${tnames.map{ x =>
          q"def ${Term.Name("visit"+x)} = _.accept(this)"
        }}}"
        val cfact = q"trait ${Type.Name("CFact")} extends ..${Seq(fact_ctor)} { ..${tpe_refts ++ ctrs.map(_.genObject(a_visitor))}}"
        val fact_obj = q"object Fact extends CFact"

        val asts = tnames.map { x =>
          val accept = q"def accept(v: $a_visitor): ${Type.Select(Term.Name("v"),Type.Name("O"+x))}"
          q"trait ${Type.Name("C"+x)} { ..${tpe_refts ++ ctrs.map(_.genFromDecl) :+ accept}}"
        }

        val template = template"..${parents :+ fact_ref} {..${out_tpes ++ new_ctrs.map{_.genVisit} ++ visit_decls}}"
        val vis = q"trait $tname extends $template"
        val companion =
          q"object ${Term.Name(name)} { ..${Seq(fact, cfact, fact_obj, c_visitor) ++ asts } }"

        val out = Term.Block(Seq(vis, companion))
        if (DEBUG) println(out.syntax)
        out
      case _ =>
        println(defn.structure)
        abort("@Visitor must annotate an trait.")
    }
  }
}

case class Ctr(lowername: String, ts: Seq[Type], t: Type.Name, old: Boolean) {
  val name = lowername.capitalize
  val ot = Type.Name("O" + t.value)
  val xs = for (i <- 1 to ts.length) yield Term.Name("x"+i)
  val params = Seq((xs,ts).zipped.map { (x,t) => param"$x: $t" })
//  val cts = ts.map{x => x match {
//    case Type.Name(n) if n == t.value => Type.Name("C"+n)
//    case _ => x
//  }}
  val from_type = ts.length match {
    case 0 => t"Boolean"
    case 1 => t"Option[..$ts]"
    case _ => t"Option[(..$ts)]"
  }
  val (from_default, from_override) =
    if (ts.isEmpty) (q"false", q"true")
    else (q"None", q"Some(..$xs)")

  def genLike = {
    val template = template"{..${Seq(genApply,genUnapply)}}"
    q"trait ${Type.Name(name + "Like")} extends $template"
  }
  def genApply =
    q"def apply(...$params): $t"

  def genUnapply =
    q"def unapply(x: $t): $from_type"

  def genField =
    q"val ${Pat.Var.Term(Term.Name(name))}: ${Type.Name(name+"Like")}"

  def genVisit =
    q"def ${Term.Name(lowername)}: ${if (ts.isEmpty) ot else Type.Function(ts,ot)}"

  def genObject(vis: Type) = {
    val ctor = ctor"${Ctor.Name(name+"Like")}"
    val accept = {
      val v = Term.Name("v")
      val selectCtr = Term.Select(v, Term.Name(lowername))
      q"def accept(v: $vis): ${Type.Select(v, ot)} = ${ if (ts.isEmpty) selectCtr else Term.Apply(selectCtr, xs)}"
    }
    val cst = template"..${Seq(Ctor.Name("C"+t.value))} { ..${Seq(accept,genFromDef)}}"
    val apply = q"def apply(...$params) = new $cst"
    val unapply = q"def unapply(x: $t) = ${Term.Select(Term.Name("x"), Term.Name("from"+name))}"
    val template = template"$ctor {..${Seq(apply,unapply)}}"
    q"object ${Term.Name(name)} extends $template"
  }
  def genFromDecl = q"def ${Term.Name("from"+name)}: $from_type = $from_default"
  def genFromDef = q"override def ${Term.Name("from"+name)} = $from_override"
}

