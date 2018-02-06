package examples

// parametric ADT
//@family trait List {
//  @adt trait List[T] {
//    def Nil: List[T]
//    def Cons: (T, List[T]) => List[T]
//  }
//}

// GADT
@family trait GadtArith {
  @adt trait Tm[A] {
    def TmZero: Tm[Int]
    def TmSucc: Tm[Int] => Tm[Int]
    def TmPred: Tm[Int] => Tm[Int]
    def TmTrue: Tm[Boolean]
    def TmFalse: Tm[Boolean]
    def TmIf: (Tm[Boolean], Tm[A], Tm[A]) => Tm[A]
    def TmIsZero: Tm[Int] => Tm[Boolean]
  }

  @visit(Tm) trait Eval {
    type OTm[A] = A
    def tmZero = 0
    def tmSucc = this(_) + 1
    def tmPred = this(_) - 1
    def tmTrue = true
    def tmFalse = false
    def tmIf[_] = (t1,t2,t3) => if (this(t1)) this(t2) else this(t3)
    def tmIsZero = this(_) == 0
  }

  @visit(Tm) trait Print {
    type OTm[A] = String
    def tmZero = "0"
    def tmSucc = _ => "succ"
    def tmPred = _ => "pred"
    def tmTrue = "true"
    def tmFalse = "false"
    def tmIf[_] = (_,_,_) => "if"
    def tmIsZero = _ => "iszero"
  }
}

object TestGadtArith extends App {
  import GadtArith._
//  println(eval(TmPred(TmTrue)))
  println(eval(TmPred(TmZero)))
  println(eval(TmIf(TmIsZero(TmZero),TmTrue,TmFalse)))
}
