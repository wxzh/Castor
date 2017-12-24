package examples

@vicase trait Arith extends Nat with Bool {
  @adt trait Tm extends super[Nat].Tm with super[Bool].Tm {
    def TmIsZero: Tm => Tm
  }
  @visit(Tm) trait Eval1 extends super[Nat].Eval1 with super[Bool].Eval1 {
    def tmIsZero = {
      case TmZero => TmTrue
      case TmSucc(t) if nv(t) => TmFalse
      case t => TmIsZero(this(t))
    }
  }
}

object Test extends Arith {
  type TmV = TmVisitor
  object eval1 extends Eval1
  val term = TmIsZero(TmIf(TmFalse,TmTrue,TmPred(TmZero)))
  def main(args: Array[String]): Unit = {
    println(eval1(term))
    println(eval1(eval1(term)))
    println(eval1(eval1(eval1(term))))
  }
}