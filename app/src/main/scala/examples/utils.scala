package examples

object utils {
  object NoRuleApplies extends Exception

  trait ITyEqv[Ty] {
    def apply(ty1: Ty, ty2: Ty): Boolean = {
      try {
        visitTy(ty1)(ty2)
      }
      catch {
        case _: MatchError => false
      }
    }

    def visitTy: Ty => Ty => Boolean
  }

  trait IIsVal[Tm] {
    def apply(t: Tm): Boolean
  }
}
