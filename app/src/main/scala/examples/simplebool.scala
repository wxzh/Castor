package examples

//  @visitor trait simplebool_TmVis[Ty] extends bool_TmVis with typed_TmVis[Ty] {
//    override def tmTrue: Tm
//    override def tmFalse: Tm
//    override def tmIf: (Tm, Tm, Tm) => Tm
//    override def tmVar: (Int, Int) => Tm
//    override def tmAbs: (String, Ty, Tm) => Tm
//    override def tmApp: (Tm, Tm) => Tm
//  }
//  @visitor trait simplebool_TyVis extends bool_TyVis with typed_TyVis {
//    override def tyArr: (Ty, Ty) => Ty
//    override def tyBool: Ty
//  }
//
//  trait simplebool_TyEqv extends simplebool_TyVis with bool_TyEqv with typed_TyEqv {
//    override type OTy = Ty => Boolean
//  }
