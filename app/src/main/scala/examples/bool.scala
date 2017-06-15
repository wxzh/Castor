package examples

import utils._

@visitor trait bool_TmVis {
  type Tm
  def tmTrue: Tm
  def tmFalse: Tm
  def tmIf: (Tm, Tm, Tm) => Tm
}

trait bool_Eval1 extends bool_TmVis {
  type OTm = Tm
  def tmTrue = throw NoRuleApplies
  def tmFalse = throw NoRuleApplies
  def tmIf = {
    case (TmTrue,t2,t3) => t2
    case (TmFalse,t2,t3) => t3
    case (t1,t2,t3) => TmIf(visitTm(t1),t2,t3)
  }
}

trait bool_Print extends bool_TmVis {
  type OTm = String
  def tmTrue = "true"
  def tmFalse = "false"
  def tmIf = "if" + visitTm(_) + " then " + visitTm(_) + " else " + visitTm(_)
}

trait bool_IsVal extends bool_TmVis {
  type OTm = Boolean
  def tmTrue = true
  def tmFalse = true
  def tmIf = (_,_,_) => false
}

@visitor trait bool_TyVis {
  type Ty
  def tyBool: Ty
}

trait bool_TyEqv extends bool_TyVis {//with ITyEqv[Ty] {
  type OTy = Ty => Boolean
  def tyBool = { case TyBool => true }
}

trait bool_Typeof[Ty] extends bool_TmVis {
  type OTm = Ty
  val tyEqv: (Ty, Ty) => Boolean
  val ty: bool_TyVis.Fact[Ty]
  def tmTrue = ty.TyBool()
  def tmFalse = ty.TyBool()
  def tmIf = (t1,t2,t3) => {
    if (tyEqv(visitTm(t1), ty.TyBool())) {
      val ty2 = visitTm(t2)
      if (tyEqv(ty2, visitTm(t3)))
        ty2
      else
        throw new Exception("arms of conditional have different types")
    }
    else {
      throw new Exception("guard of conditional is not a boolean")
    }
  }
}

trait bool_PrintTy extends bool_TyVis {
  type OTy = String
  def tyBool = "Bool"
}
