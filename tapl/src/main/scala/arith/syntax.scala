package tapl.arith

import examples._
import util.Document
import util.Document._
import util.Print._

@vicase trait Binding {
  trait Command
  case class Bind(n: String, b: Binding) extends Command

  @adt trait Binding {
    def NameBind: Binding
  }

  @default(Binding) trait BindingShift {
    type OBinding = Int => Binding
    def otherwise = b => _ => b
  }

  @default(Binding) trait PBinding {
    type OBinding = Context => Document
    def otherwise = _ => _ => empty
  }

  case class Context(l: List[(String, Binding)] = List()) {
    val length: Int =
      l.length

    def addBinding(s: String, bind: Binding): Context =
      Context((s, bind) :: l)

    def addName(s: String): Context =
      addBinding(s, NameBind)

    def isNameBound(s: String): Boolean =
      l.exists { _._1 == s }

    def pickFreshName(n: String): (Context, String) =
      if (isNameBound(n))
        pickFreshName(n + "_")
      else
        (Context((n, NameBind) :: l), n)

    def index2Name(i: Int): String =
      l(i)._1

    def name2index(s: String): Int =
      l.indexWhere { _._1 == s } match {
        case -1 => throw new Exception("identifier " + s + " is unbound")
        case i  => i
      }

    def getBinding(i: Int): Binding =
      bindingShift(l(i)._2)(i+1)

  }
}

@adts(Binding)
@ops(BindingShift, PBinding)
@vicase trait Term extends Binding {
  @adt trait Tm

  case class NoRuleApplies(t: Tm = null) extends Exception("No rule applies for term: " + t)

  case class Eval(t: Tm) extends Command

  def ptm(t: Tm, ctx: Context): Document = ptmTerm(t)(true,ctx)

  @default(Tm) trait PtmTerm {
    type OTm = (Boolean, Context) => Document
    def otherwise = ptmAppTerm(_)
  }

  @default(Tm) trait PtmAppTerm {
    type OTm = (Boolean, Context) => Document
    override def otherwise = ptmATerm(_)
  }

  @default(Tm) trait PtmATerm {
    type OTm = (Boolean, Context) => Document
    override def otherwise = t => (_,ctx) => "(" :: ptmTerm(t)(true,ctx) :: ")"
  }

  @default(Tm) trait IsVal {
    type OTm = Boolean
    def otherwise = _ => false
  }

  @default(Tm) trait Eval1 {
    type OTm = Context => Tm
    def otherwise = t => throw NoRuleApplies(t)
  }

  @default(Tm) trait TmMap {
    type OTm = ((Int,Int,Int) => Tm, Int) => Tm
    def otherwise = t => (_,_) => t
  }

  def eval(ctx: Context, t: Tm): Tm =
    try {
      val t1 = eval1(t)(ctx)
      eval(ctx,t1)
    } catch {
      case _: NoRuleApplies => t
    }
}

@adts(Binding)
@ops(BindingShift, PBinding)
@vicase trait Nat extends Term {

  @adt trait Tm extends super.Tm {
    def TmZero: Tm
    def TmSucc: Tm => Tm
    def TmPred: Tm => Tm
  }

  @default(Tm) trait TmMap extends super.TmMap {
    override def tmSucc = t => (onvar,c) => TmSucc(this(t)(onvar,c))
    override def tmPred = t => (onvar,c) => TmPred(this(t)(onvar,c))
  }

  @default(Tm) trait PtmTerm extends super.PtmTerm

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm {
    override def tmPred = t => (_,ctx) =>
      "pred " :: ptmATerm(t)(false,ctx)
  }

  @default(Tm) trait PtmATerm extends super.PtmATerm {
    override def tmZero = (_,_) => "0"
    override def tmSucc = t => (_,ctx) => {
      def pf(i: Int, t: Tm): Document = t match {
        case TmZero =>
          i.toString
        case TmSucc(s) =>
          pf(i + 1, s)
        case _ =>
          "(succ " :: this(t)(false,ctx) :: ")"
      }
      pf(1, t)
    }
  }

  @default(Tm) trait IsVal extends super.IsVal {
    override def tmZero = true
  }

  def isNumericVal(t: Tm): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => isNumericVal(t1)
    case _ => false
  }

  @default(Tm) trait Eval1 extends super.Eval1 {
    override def tmSucc = t => ctx =>
      TmSucc(this(t)(ctx))
    override def tmPred = {
      case TmZero => _ => TmZero
      case TmSucc(t) if isNumericVal(t) => _ => t
      case t => ctx => TmPred(this(t)(ctx))
    }
  }
}

@adts(Binding)
@ops(BindingShift, PBinding)
@vicase trait Bool extends Term {

  @adt trait Tm extends super.Tm {
    def TmTrue: Tm
    def TmFalse: Tm
    def TmIf: (Tm,Tm,Tm) => Tm
  }

  @default(Tm) trait TmMap extends super.TmMap {
    override def tmIf = (t1,t2,t3) => (onvar,c) => TmIf(this(t1)(onvar,c),this(t2)(onvar,c),this(t3)(onvar,c))
  }

  @default(Tm) trait PtmTerm extends super.PtmTerm {
    override def tmIf = (t1,t2,t3) => (outer,ctx) => {
      val ifB = g2("if" :/: this(t1)(outer,ctx))
      val thenB = g2("then" :/: this(t2)(outer,ctx))
      val elseB = g2("else" :/: this(t3)(outer,ctx))
      g0(ifB :/: thenB :/: elseB)
    }
  }

  @default(Tm) trait PtmATerm extends super.PtmATerm {
    override def tmTrue = (_,_) => "true"
    override def tmFalse = (_,_) => "false"
  }

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm


  @default(Tm) trait Eval1 extends super.Eval1 {
    override def tmIf = {
      case (TmTrue,t2,_) => _ => t2
      case (TmFalse,_,t3) => _ => t3
      case (t1,t2,t3) => ctx => TmIf(this(t1)(ctx),t2,t3)
    }
  }

  @default(Tm) trait IsVal extends super.IsVal {
    override def tmTrue = true
    override def tmFalse = true
  }
}


@adts(Binding)
@ops(BindingShift, PBinding)
@vicase trait Arith extends Nat with Bool {
  @adt trait Tm extends super[Nat].Tm with super[Bool].Tm {
    def TmIsZero: Tm => Tm
  }

  @visit(Tm) trait TmMap extends super[Nat].TmMap with super[Bool].TmMap {
    def tmIsZero = t => (onvar,c) => TmIsZero(this(t)(onvar,c))
  }

  @default(Tm) trait PtmTerm extends super[Nat].TmDefault with super[Bool].PtmTerm

  @visit(Tm) trait PtmAppTerm extends super[Nat].PtmAppTerm with super[Bool].TmDefault {
    override def tmIsZero = t => (_,ctx) =>
      "iszero " :: ptmATerm(t)(false,ctx)
  }

  @default(Tm) trait PtmATerm extends super[Nat].PtmATerm with super[Bool].PtmATerm

  @visit(Tm) trait Eval1 extends super[Nat].Eval1 with super[Bool].Eval1 {
    def tmIsZero = {
      case TmZero => _ => TmTrue
      case TmSucc(nv) if isNumericVal(nv) => _ => TmFalse
      case t => ctx => TmIsZero(this(t)(ctx))
    }
  }

  @default(Tm) trait IsVal extends super[Nat].IsVal with super[Bool].IsVal {
    override def apply(t: Tm) = isNumericVal(t) || t.accept(this)
  }
}