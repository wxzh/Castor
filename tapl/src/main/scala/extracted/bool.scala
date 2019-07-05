package tapl.extracted

import examples._
import util.Document._
import util.Print._

@adts(Binding)
@ops(BindingShift, PBinding)
@family trait Bool extends Term {

  @adt trait Tm extends super.Tm {
    case object TmTrue
    case object TmFalse
    case class TmIf(t1: Tm, t2: Tm, t3: Tm)
  }

  @default(Tm) trait TmMap extends super.TmMap {
    override def tmIf = x => (onvar,c) => TmIf(this(x.t1)(onvar,c),this(x.t2)(onvar,c),this(x.t3)(onvar,c))
  }

  @default(Tm) trait PtmTerm extends super.PtmTerm {
    override def tmIf = x => (outer,ctx) => {
      val ifB = g2("if" :/: this(x.t1)(outer,ctx))
      val thenB = g2("then" :/: this(x.t2)(outer,ctx))
      val elseB = g2("else" :/: this(x.t3)(outer,ctx))
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
      case TmIf(TmTrue,t2,_) => _ => t2
      case TmIf(TmFalse,_,t3) => _ => t3
      case TmIf(t1,t2,t3) => ctx => TmIf(this(t1)(ctx),t2,t3)
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
    case object TyBool
  }

  @default(Ty) trait PtyType extends super.PtyType

  @visit(Ty) trait PtyAType extends super.PtyAType {
    def tyBool = (_,_) => "Bool"
  }

  @visit(Tm) trait Typeof extends super.Typeof {
    def tmTrue = _ => TyBool
    def tmFalse = _ => TyBool
    def tmIf = x => ctx =>
      if (this(x.t1)(ctx) == TyBool) {
        val ty2 = this(x.t2)(ctx)
        if (ty2 == this(x.t3)(ctx)) {
          ty2
        } else {
          throw new Exception("arms of conditional " + TmIf(x.t1,x.t2,x.t3) + " have different types")
        }
      } else {
        throw new Exception("guard of conditional " + TmIf(x.t1,x.t2,x.t3) + " is not a boolean")
      }
  }

  @visit(Ty) trait TyEqv extends super.TyEqv {
    def tyBool = { case TyBool => true }
  }

  @visit(Ty) trait Subtype extends super.Subtype with TyEqv
}
