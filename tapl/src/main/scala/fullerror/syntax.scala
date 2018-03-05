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
    def TmTry: (Tm,Tm) => Tm
    def TmError: Tm
  }

  @adt trait Ty extends super[TyBool].Ty with super[BotJoinMeet].Ty


  @visit(Tm) trait TmMap extends super[TyBool].TmMap with super[BotJoinMeet].TmMap {
    def tmTry = (t1,t2) => (onvar,c) => TmTry(this(t1)(onvar,c),this(t2)(onvar,c))
    def tmError = (_,_) => TmError
  }

  @default(Tm) trait PtmTerm extends super[TyBool].PtmTerm with super[BotJoinMeet].PtmTerm {
    override def tmTry = (t1,t2) => (outer,ctx) =>
      g0("try " :: this(t1)(false, ctx) :/: "with " :: this(t1)(false, ctx))

    override def tmAbs = super.tmAbs
  }

  @default(Tm) trait PtmATerm extends super[TyBool].PtmATerm with super[BotJoinMeet].PtmATerm {
    override def tmError = (_,_) =>
      "error"
  }

  @default(Tm) trait PtmAppTerm extends super[TyBool].PtmAppTerm with super[BotJoinMeet].PtmAppTerm

  @default(Tm) trait Eval1 extends super[TyBool].Eval1 with super[TyVarBinding].Eval1 with super[BotJoinMeet].Eval1 {
    override def tmApp = {
      case (TmError, t2) => _ =>
        TmError
      case (v1, TmError) if isVal(v1) => _ =>
        TmError
      case (t1,t2) => ctx =>
        super.tmApp(t1,t2)(ctx)
    }
    override def tmIf = {
      case (TmError,t2,t3) => _ =>
        TmError
      case (t1,t2,t3) => ctx =>
        super.tmIf(t1,t2,t3)(ctx)
    }
  }

  @visit(Tm) trait IsVal extends super[TyBool].IsVal with super[BotJoinMeet].IsVal {
    def tmTry = (t1,t2) => false
    def tmError = false
  }

  @visit(Tm) trait Typeof extends super[TyBool].Typeof with super[BotJoinMeet].Typeof {
    override def tmIf = (t1,t2,t3) => ctx =>
      if (subtype(this(t1)(ctx))(TyBool)) {
        join(this(t2)(ctx))(this(t3)(ctx))
      } else {
        throw new Exception("guard of conditional " + TmIf(t1,t2,t3) + " is not a boolean")
      }
    def tmError = _ =>
      TyBot
    def tmTry = (t1,t2) => ctx =>
      join(this(t1)(ctx))(this(t2)(ctx))
  }

  @visit(Ty) trait PtyType extends super[TyBool].PtyType with super[BotJoinMeet].PtyType
  @visit(Ty) trait PtyAType extends super[TyBool].PtyAType with super[BotJoinMeet].PtyAType
  @default(Ty) trait PtyArrowType extends super[BotJoinMeet].PtyArrowType

  @visit(Ty) trait TyEqv extends super[TyBool].TyEqv with super[BotJoinMeet].TyEqv
  @visit(Ty) trait Subtype extends super[TyBool].Subtype with super[BotJoinMeet].Subtype
  @default(Ty) trait Join extends super[BotJoinMeet].Join
  @default(Ty) trait Meet extends super[BotJoinMeet].Meet
}
