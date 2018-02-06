package tapl.extracted

import examples._
import util.Document
import util.Document._
import util.Print._

@family trait Binding {
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
@family trait Term extends Binding {
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

@adts(Binding,Tm)
@ops(BindingShift, PBinding, Eval1, IsVal, PtmTerm, PtmATerm, PtmAppTerm, TmMap)
@family trait Type extends Term {
  @adt trait Ty

  @default(Ty) trait PtyType {
    type OTy = (Boolean,Context) => Document
    def otherwise = ptyAType(_)
  }

  @default(Ty) trait PtyAType {
    type OTy = (Boolean,Context) => Document
    def otherwise = ty => "(" :: ptyType(ty)(_,_) :: ")"
  }

  def ptyTy(ty: Ty, ctx: Context) = ptyType(ty)(true,ctx)

  @visit(Tm) trait Typeof {
    type OTm = Context => Ty
  }

  //  trait TyMap extends TyDefault {_: TyV =>
  //    type T = Int => Ty
  //    val onVar: (Int,Int,Int) => Ty
  //    def otherwise = ty => _ => ty
  //  }

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
    def VarBind: Ty => Binding
  }

  @default(Binding) trait GetTypeFromBind {
    type OBinding = Ty
    override def varBind = identity
    def otherwise = _ => throw new Exception("Wrong kind of binding")
  }

  @visit(Binding) trait PBinding extends super.PBinding {
    def varBind = ty => ctx => ": " :: ptyType(ty)(true,ctx)
  }

  @default(Binding) trait BindingShift extends super.BindingShift
}


@family
@adts(Tm)
@ops(PtmTerm,PtmATerm,PtmAppTerm,IsVal,TmMap)
trait TmAbbBinding extends Binding with VarApp {
  @adt trait Binding extends super.Binding {
    def TmAbbBind: Tm => Binding
  }

  @visit(Tm) trait Eval1 extends super.Eval1 {
    override def tmVar = (x,_) => ctx =>
      ctx.getBinding(x) match {
        case TmAbbBind(t) => t
        case _ => throw NoRuleApplies()
      }
  }

  @visit(Binding) trait BindingShift extends super.BindingShift {
    def tmAbbBind = t => d =>
      TmAbbBind(termShift(d, t))
  }

  @visit(Binding) trait PBinding extends super.PBinding {
    def tmAbbBind = tm => ctx =>
      "= " :: ptm(tm,ctx)
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
@ops(Eval1,IsVal,PtmTerm,PtmATerm,TmMap,Typeof,PtmAppTerm,PtyType,PtyAType,PtyArrowType,TyEqv,Subtype)
trait TyVarBinding extends Typed with VarBinding {

  @adt trait Binding extends super.Binding {
    def TyVarBind: Binding
    def TmAbbBind: (Tm, Option[Ty]) => Binding
    def TyAbbBind: Ty => Binding
  }

  @visit(Binding) trait PBinding extends super.PBinding {
    def tyVarBind = ctx =>
      empty

    def tyAbbBind = ty => ctx =>
      "= " :: ptyTy(ty,ctx)

    def tmAbbBind = (t, tyT) => ctx =>
      "= " :: ptm(t,ctx)
  }

  @visit(Binding) trait PBindingTy extends PBinding {
    override def tyAbbBind = _ => _ =>
      ":: *"
    override def tmAbbBind = {
      case (t, Some(ty)) => ctx =>
        ": " :: ptyTy(ty,ctx)
      case (t, None) => ctx =>
        ": " :: ptyTy(typeof(t)(ctx),ctx)
    }
  }

  @default(Binding) trait GetTypeFromBind extends super.GetTypeFromBind {
    override def tmAbbBind = {
      case (_, Some(ty)) => ty
      case (_, None)     => throw new Exception("No type recorder for variable")
    }
  }

  @default(Binding) trait BindingShift extends super.BindingShift {
    override def tmAbbBind = (t, ty) => d => TmAbbBind(termShift(d,t), ty)
  }

  @default(Binding) trait CheckBinding {
    type OBinding = Context => Binding
    def otherwise = bind => _ => bind
    override def tmAbbBind = {
      case (t, None) => ctx => TmAbbBind(t, Some(typeof(t)(ctx)))
      case (t, Some(ty)) => ctx => {
        val ty1 = typeof(t)(ctx)
        if (tyEqv(ty)(ty1))
          TmAbbBind(t,Some(ty))
        else
          throw new Exception("type of binding doesn't match declared type")
      }
    }
  }
}
