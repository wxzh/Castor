package tapl.extracted

import examples._
import util.Print._

@family
@adts(Binding)
@ops(BindingShift, PBinding)
trait Str extends Term {
  @adt trait Tm extends super.Tm {
    def TmString: String => Tm
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmString = s => (_,_) => TmString(s)
  }

  @default(Tm) trait PtmTerm extends super.PtmTerm

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm

  @visit(Tm) trait PtmATerm extends super.PtmATerm {
    def tmString = s => (_,_) => "\"" :: s :: "\""
  }

  @visit(Tm) trait IsVal extends super.IsVal {
    def tmString = _ => true
  }

  @default(Tm) trait Eval1 extends super.Eval1
}

@family
@adts(Binding,Tm)
@ops(BindingShift,PBinding,TmMap,PtmTerm,PtmAppTerm,PtmATerm,IsVal,Eval1)
trait TyStr extends Type with Str {

  @adt trait Ty extends super.Ty {
    def TyString: Ty
  }

  @default(Ty) trait PtyType extends super.PtyType

  @visit(Ty) trait PtyAType extends super.PtyAType {
    def tyString = (_, _) => "String"
  }

  @visit(Tm) trait Typeof extends super.Typeof {
    def tmString = _ => _ => TyString
  }

  @visit(Ty) trait TyEqv extends super.TyEqv {
    def tyString = { case TyString => true }
  }

  @visit(Ty) trait Subtype extends TyEqv with super.Subtype
}