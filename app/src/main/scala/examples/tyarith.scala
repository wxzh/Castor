package examples

@visitor trait tyarith_TyVis extends bool_TyVis with nat_TyVis {
  override def tyNat: Ty
  override def tyBool: Ty
}

trait tyarith_PrintTy extends tyarith_TyVis with nat_PrintTy with bool_PrintTy {
  override type OTy = String
}

trait tyarith_TyEqv extends tyarith_TyVis with nat_TyEqv with bool_TyEqv {
  override type OTy = Ty => Boolean
}

trait tyarith_Typeof[Ty] extends arith_TmVis with bool_Typeof[Ty] with nat_Typeof[Ty] {
  override type OTm = Ty
  override val ty: tyarith_TyVis.Fact[Ty]
  override def tmIsZero = t =>
    if (tyEqv(visitTm(t),ty.TyNat())) ty.TyBool()
    else throw new Exception("argument of iszero is not a number")
}
