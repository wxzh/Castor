import org.scalatest._
import tapl.rcdsubbot.RcdSubBot._

class RcdSubTopTest extends FreeSpec with Matchers {
//  val arr = TyArr(Ty)
  val rcd = TyRecord(List(("x",TyArr(TyTop,TyTop))))

  "Subtype" - {
    "REFL" in {
      subtype(rcd)(rcd) shouldBe true
      subtype(TyTop)(TyTop) shouldBe true
      subtype(TyBot)(TyBot) shouldBe true
    }
    "BOT" in {
      subtype(TyBot)(rcd) shouldBe true
      subtype(rcd)(TyBot) shouldBe false
    }
    "TOP" in {
      subtype(rcd)(TyTop) shouldBe true
      subtype(TyTop)(rcd) shouldBe false
    }
    "ARROW" in {
      subtype(TyArr(TyTop,TyTop))(TyArr(TyTop,rcd)) shouldBe false
      subtype(TyArr(TyTop,TyTop))(TyArr(rcd,TyTop)) shouldBe true
      subtype(TyArr(TyBot,TyBot))(TyArr(rcd,TyBot)) shouldBe false
      subtype(TyArr(TyBot,TyBot))(TyArr(TyBot,rcd)) shouldBe true
      subtype(TyArr(TyTop,TyBot))(TyArr(rcd,rcd)) shouldBe true
    }
  }
}