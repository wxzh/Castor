package examples

import utils._

@family trait Bool extends Term {
  @adt trait Tm extends super.Tm {
    def TmTrue: Tm
    def TmFalse: Tm
    def TmIf: (Tm,Tm,Tm) => Tm
  }
  @default(Tm) trait Eval1 extends super.Eval1 {
    override def tmIf = {
      case (TmTrue,t2,_) => t2
      case (TmFalse,_,t3) => t3
      case (t1,t2,t3) => TmIf(this(t1),t2,t3)
    }
  }
}

@adts(Tm) @ops(Eval1)
@family trait TyBool extends Bool with Type {
  @adt trait Ty extends super.Ty {
    def TyBool: Ty
  }
  @visit(Tm) trait Typeof extends super.Typeof {
    def tmTrue = Some(TyBool)
    def tmFalse = Some(TyBool)
    def tmIf = (t1,t2,t3) => for {
      TyBool <- this(t1)
      ty2 <- this(t2)
      ty3 <- this(t3)
      if (ty2 == ty3)
    } yield(ty2)
  }
}
