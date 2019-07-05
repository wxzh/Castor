package tapl.extracted

import examples._
import util.Document
import util.Document._
import util.Print._

@family trait Binding {
  trait Command
  case class Bind(n: String, b: Binding) extends Command

  @adt trait Binding {
    case object NameBind
  }

  @default(Binding) trait BindingShift {
    type OBinding = Int => Binding
    def binding = b => _ => b
  }

  @default(Binding) trait PBinding {
    type OBinding = Context => Document
    def binding = _ => _ => empty
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
@family trait Term extends Binding {
  @adt trait Tm

  case class NoRuleApplies(t: Tm = null) extends Exception("No rule applies for term: " + t)

  case class Eval(t: Tm) extends Command

  def ptm(t: Tm, ctx: Context): Document = ptmTerm(t)(true,ctx)

  @default(Tm) trait PtmTerm {
    type OTm = (Boolean, Context) => Document
    def tm = ptmAppTerm(_)
  }

  @default(Tm) trait PtmAppTerm {
    type OTm = (Boolean, Context) => Document
    override def tm = ptmATerm(_)
  }

  @default(Tm) trait PtmATerm {
    type OTm = (Boolean, Context) => Document
    override def tm = t => (_,ctx) => "(" :: ptmTerm(t)(true,ctx) :: ")"
  }

  @default(Tm) trait IsVal {
    type OTm = Boolean
    def tm = _ => false
  }

  @default(Tm) trait Eval1 {
    type OTm = Context => Tm
    def tm = t => throw NoRuleApplies(t)
  }

  @default(Tm) trait TmMap {
    type OTm = ((Int,Int,Int) => Tm, Int) => Tm
    def tm = t => (_,_) => t
  }

  def eval(ctx: Context, t: Tm): Tm =
    try {
      val t1 = eval1(t)(ctx)
      eval(ctx,t1)
    } catch {
      case _: NoRuleApplies => t
    }
}

@adts(Binding,Tm)
@ops(BindingShift, PBinding, Eval1, IsVal, PtmTerm, PtmATerm, PtmAppTerm, TmMap)
@family trait Type extends Term {
  @adt trait Ty

  @default(Ty) trait PtyType {
    type OTy = (Boolean,Context) => Document
    def ty = ptyAType(_)
  }

  @default(Ty) trait PtyAType {
    type OTy = (Boolean,Context) => Document
    def ty = ty => "(" :: ptyType(ty)(_,_) :: ")"
  }

  def ptyTy(ty: Ty, ctx: Context) = ptyType(ty)(true,ctx)

  @visit(Tm) trait Typeof {
    type OTm = Context => Ty
  }

  @visit(Ty) trait TyEqv {
    type OTy = Ty => Boolean
    override def apply(ty1: Ty) = ty2 => {
      try { ty1.accept(this)(ty2) }
      catch { case _: MatchError => false }
    }
  }

  @visit(Ty) trait Subtype extends TyEqv
}

@family
@adts(Tm,Ty)
@ops(Eval1, IsVal, PtmATerm, PtmAppTerm, PtmTerm, TmMap, PtyType, PtyAType, TyEqv, Subtype, Typeof)
trait VarBinding extends Binding with Type {
  @adt trait Binding extends super.Binding {
    case class VarBind(ty: Ty)
  }

  @default(Binding) trait GetTypeFromBind {
    type OBinding = Ty
    override def varBind = _.ty
    def binding = _ => throw new Exception("Wrong kind of binding")
  }

  @visit(Binding) trait PBinding extends super.PBinding {
    def varBind = x => ctx => ": " :: ptyType(x.ty)(true,ctx)
  }

  @default(Binding) trait BindingShift extends super.BindingShift
}


@family
@adts(Tm)
@ops(PtmTerm,PtmATerm,PtmAppTerm,IsVal,TmMap)
trait TmAbbBinding extends Binding with VarApp {
  @adt trait Binding extends super.Binding {
    case class TmAbbBind(t: Tm)
  }

  @visit(Tm) trait Eval1 extends super.Eval1 {
    override def tmVar = x => ctx =>
      ctx.getBinding(x.i) match {
        case TmAbbBind(t) => t
        case _ => throw NoRuleApplies()
      }
  }

  @visit(Binding) trait BindingShift extends super.BindingShift {
    def tmAbbBind = x => d =>
      TmAbbBind(termShift(d, x.t))
  }

  @visit(Binding) trait PBinding extends super.PBinding {
    def tmAbbBind = x => ctx =>
      "= " :: ptm(x.t,ctx)
  }

  def evalBinding(ctx: Context, bind: Binding): Binding = bind match {
    case TmAbbBind(t) =>
      TmAbbBind(eval(ctx,t))
    case b =>
      b
  }
}

@family
@adts(Tm,Ty)
@ops(IsVal,PtmTerm,PtmATerm,TmMap,Typeof,PtmAppTerm,PtyType,PtyAType,PtyArrowType,TyEqv,Subtype)
trait TyVarBinding extends Typed with VarBinding {

  @adt trait Binding extends super.Binding {
    case object TyVarBind
    case class TmAbbBind(t: Tm, ty: Option[Ty])
    case class TyAbbBind(ty: Ty)
  }

  @visit(Tm) trait Eval1 extends super.Eval1 {
    override def tmVar = x => ctx =>
      ctx.getBinding(x.i) match {
        case TmAbbBind(t1, _) => t1
        case _ => throw new NoRuleApplies
      }
  }

  @visit(Binding) trait PBinding extends super.PBinding {
    def tyVarBind = ctx =>
      empty

    def tyAbbBind = x => ctx =>
      "= " :: ptyTy(x.ty,ctx)

    def tmAbbBind = x => ctx =>
      "= " :: ptm(x.t,ctx)
  }

  @visit(Binding) trait PBindingTy extends PBinding {
    override def tyAbbBind = _ => _ =>
      ":: *"
    override def tmAbbBind = {
      case TmAbbBind(t, Some(ty)) => ctx =>
        ": " :: ptyTy(ty,ctx)
      case TmAbbBind(t, None) => ctx =>
        ": " :: ptyTy(typeof(t)(ctx),ctx)
    }
  }

  @default(Binding) trait GetTypeFromBind extends super.GetTypeFromBind {
    override def tmAbbBind = {
      case TmAbbBind(_, Some(ty)) => ty
      case TmAbbBind(_, None)     => throw new Exception("No type recorder for variable")
    }
  }

  @default(Binding) trait BindingShift extends super.BindingShift {
    override def tmAbbBind = x => d => TmAbbBind(termShift(d,x.t), x.ty)
  }

  @default(Binding) trait CheckBinding {
    type OBinding = Context => Binding
    def binding = bind => _ => bind
    override def tmAbbBind = {
      case TmAbbBind(t, None) => ctx => TmAbbBind(t, Some(typeof(t)(ctx)))
      case TmAbbBind(t, Some(ty)) => ctx => {
        val ty1 = typeof(t)(ctx)
        if (tyEqv(ty)(ty1))
          TmAbbBind(t,Some(ty))
        else
          throw new Exception("type of binding doesn't match declared type")
      }
    }
  }
}
