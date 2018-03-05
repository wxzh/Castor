import org.scalatest._
import tapl.fullsub.FullSub._

class FullSubTest extends FreeSpec with Matchers {
  val t1 = TmRecord(List(("x", TmZero)))
  val t2 = TmRecord(List(("x", TmTrue)))

  "Typeof" - {
    "t1" in {
      typeof(t1)(Context()) shouldBe TyRecord(List(("x", TyNat)))
      typeof(t2)(Context()) shouldBe TyRecord(List(("x", TyBool)))
    }
  }


  "Join" - {
    "Nat and Nat" in {
      join(TyNat)(TyNat) shouldBe TyNat
    }
    "Nat and Bool" in {
      join(TyNat)(TyBool) shouldBe TyTop
    }
    "Bool and Nat" in {
      join(TyBool)(TyNat) shouldBe TyTop
    }
  }
//  "Join" - {
//    "t" in {
//      join(typeof(t1)(Context()))(typeof(t2)(Context())) shouldBe TyRecord(List(("x", TyTop)))
//    }
//  }
}