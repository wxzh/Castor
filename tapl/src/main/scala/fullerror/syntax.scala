package tapl.fullerror


import tapl.extracted._
import util.Print._
import tapl.bot._
import examples._

@family
@adts(Binding)
@ops(BindingShift, PBinding, PBindingTy, GetTypeFromBind, CheckBinding)
trait FullError extends TyBool with TyVarBinding with BotJoinMeet {

  @adt trait Tm extends super[TyBool].Tm with super[BotJoinMeet].Tm {
    case class TmTry(t1: Tm, t2: Tm)
    case object TmError
  }

  @adt trait Ty extends super[TyBool].Ty with super[BotJoinMeet].Ty


  @visit(Tm) trait TmMap extends super[TyBool].TmMap with super[BotJoinMeet].TmMap {
    def tmTry = x => (onvar,c) => TmTry(this(x.t1)(onvar,c),this(x.t2)(onvar,c))
    def tmError = (_,_) => TmError
  }

  @default(Tm) trait PtmTerm extends super[TyBool].PtmTerm with super[BotJoinMeet].PtmTerm {
    override def tmTry = {
      case TmTry(t1,t2) => (outer,ctx) => g0("try " :: this(t1)(false, ctx) :/: "with " :: this(t1)(false, ctx)) }
//TODO: why the following lines do not type-check?
//    x => (outer,ctx) =>
//     g0("try " :: this (x.t1)(false, ctx) :/: "with " :: this (x.t1)(false, ctx))

    override def tmAbs = super.tmAbs
  }

  @default(Tm) trait PtmATerm extends super[TyBool].PtmATerm with super[BotJoinMeet].PtmATerm {
    override def tmError = (_,_) =>
      "error"
  }

  @default(Tm) trait PtmAppTerm extends super[TyBool].PtmAppTerm with super[BotJoinMeet].PtmAppTerm

  @default(Tm) trait Eval1 extends super[TyBool].Eval1 with super[TyVarBinding].Eval1 with super[BotJoinMeet].Eval1 {
    override def tmApp = {
      case TmApp(TmError, t2) => _ =>
        TmError
      case TmApp(v1, TmError) if isVal(v1) => _ =>
        TmError
      case x => ctx =>
        super.tmApp(x)(ctx)
    }
    override def tmIf = {
      case TmIf(TmError,t2,t3) => _ =>
        TmError
      case x => ctx =>
        super.tmIf(x)(ctx)
    }
  }

  @visit(Tm) trait IsVal extends super[TyBool].IsVal with super[BotJoinMeet].IsVal {
    def tmTry = _ => false
    def tmError = false
  }

  @visit(Tm) trait Typeof extends super[TyBool].Typeof with super[BotJoinMeet].Typeof {
    override def tmIf = x => ctx =>
      if (subtype(this(x.t1)(ctx))(TyBool)) {
        join(this(x.t2)(ctx))(this(x.t3)(ctx))
      } else {
        throw new Exception("guard of conditional " + x + " is not a boolean")
      }
    def tmError = _ =>
      TyBot
    def tmTry = x => ctx =>
      join(this(x.t1)(ctx))(this(x.t2)(ctx))
  }

  @visit(Ty) trait PtyType extends super[TyBool].PtyType with super[BotJoinMeet].PtyType
  @visit(Ty) trait PtyAType extends super[TyBool].PtyAType with super[BotJoinMeet].PtyAType
  @default(Ty) trait PtyArrowType extends super[BotJoinMeet].PtyArrowType

  @visit(Ty) trait TyEqv extends super[TyBool].TyEqv with super[BotJoinMeet].TyEqv
  @visit(Ty) trait Subtype extends super[TyBool].Subtype with super[BotJoinMeet].Subtype
  @default(Ty) trait Join extends super[BotJoinMeet].Join
  @default(Ty) trait Meet extends super[BotJoinMeet].Meet
}
