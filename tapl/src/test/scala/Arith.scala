import org.scalatest._
import tapl.arith.Arith._
import util.Print

class ArithTest extends FreeSpec with Matchers {
  val t1 = TmIf(TmTrue,TmIf(TmFalse,TmSucc(TmSucc(TmZero)),TmPred(TmSucc(TmSucc(TmSucc(TmSucc(TmZero)))))),TmSucc(TmSucc(TmSucc(TmZero))))
  val t2 = TmSucc(TmSucc(TmSucc(TmZero)))

  "Eval" - {
    "t" in {
      eval(Context(),t1) shouldBe t2
    }
  }
}
