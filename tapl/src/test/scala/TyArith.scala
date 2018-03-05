import org.scalatest._
import tapl.tyarith.TyArith._
import util.Print

class TyArithTest extends FreeSpec with Matchers {
  val t1 = TmIf(TmTrue,TmIf(TmFalse,TmSucc(TmSucc(TmZero)),TmPred(TmSucc(TmSucc(TmSucc(TmSucc(TmZero)))))),TmSucc(TmSucc(TmSucc(TmZero))))
  val t2 = TmSucc(TmSucc(TmSucc(TmZero)))

  "TyEqv" - {
    "Nat and Bool" in {
      tyEqv(TyNat)(TyBool) shouldBe false
    }
    "Bool and Nat" in {
      tyEqv(TyBool)(TyNat) shouldBe false
    }
    "Nat and Nat" in {
      tyEqv(TyNat)(TyNat) shouldBe true
    }
  }
}
