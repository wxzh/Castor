package examples

// parametric ADT
//@vicase trait List {
//  @adt trait List[T] {
//    def Nil: List[T]
//    def Cons: (T, List[T]) => List[T]
//  }
//}

// GADT
@vicase trait TypedArith {
  @adt trait Tm[T] {
    def TmZero: Tm[Int]
    def TmSucc: Tm[Int] => Tm[Int]
    def TmPred: Tm[Int] => Tm[Int]
    def TmTrue: Tm[Boolean]
    def TmFalse: Tm[Boolean]
    def TmIf: (Tm[Boolean], Tm[T], Tm[T]) => Tm[T]
    def TmIsZero: Tm[Int] => Tm[Boolean]
  }

  trait Eval extends TmVisitor { _: TmV =>
    type OTm[T] = T
    def tmZero = 0
    def tmSucc = this(_) + 1
    def tmPred = this(_) - 1
    def tmTrue = true
    def tmFalse = false
    def tmIf[_] = (e1,e2,e3) =>
      if (this(e1)) this(e2) else this(e3)
    def tmIsZero = this(_) == 0
  }
}
