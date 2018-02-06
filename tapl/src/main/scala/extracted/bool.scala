package tapl.extracted

import examples._
import util.Document._
import util.Print._

@adts(Binding)
@ops(BindingShift, PBinding)
@family trait Bool extends Term {

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

@family
@adts(Binding,Tm)
@ops(BindingShift, PBinding, Eval1, IsVal, PtmTerm, PtmATerm, PtmAppTerm, TmMap)
trait TyBool extends Type with Bool {
  @adt trait Ty extends super.Ty {
    def TyBool: Ty
  }

  @default(Ty) trait PtyType extends super.PtyType

  @visit(Ty) trait PtyAType extends super.PtyAType {
    def tyBool = (_,_) => "Bool"
  }

  @visit(Tm) trait Typeof extends super.Typeof {
    def tmTrue = _ => TyBool
    def tmFalse = _ => TyBool
    def tmIf = (t1,t2,t3) => ctx =>
      if (this(t1)(ctx) == TyBool) {
        val ty2 = this(t2)(ctx)
        if (ty2 == this(t3)(ctx)) {
          ty2
        } else {
          throw new Exception("arms of conditional " + TmIf(t1, t2, t3) + " have different types")
        }
      } else {
        throw new Exception("guard of conditional " + TmIf(t1,t2,t3) + " is not a boolean")
      }
  }

  @visit(Ty) trait TyEqv extends super.TyEqv {
    def tyBool = { case TyBool => true }
  }

  @visit(Ty) trait Subtype extends super.Subtype with TyEqv
}
