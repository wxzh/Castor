package examples

import utils._

@vicase trait Bool extends Term {
  @adt trait Tm extends super.Tm {
    def TmTrue: Tm
    def TmFalse: Tm
    def TmIf: (Tm,Tm,Tm) => Tm
  }
  @visitor trait Eval1 extends TmDefault with super.Eval1 {_: TmV =>
    override def tmIf = {
      case (TmTrue,t2,_) => t2
      case (TmFalse,_,t3) => t3
      case (t1,t2,t3) => TmIf(this(t1),t2,t3)
    }
  }
}