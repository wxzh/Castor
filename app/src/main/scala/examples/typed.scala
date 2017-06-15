package examples

//  @visitor
//  trait typed_TyVis {
//    type Ty
//    def tyArr: (Ty, Ty) => Ty
//  }
//
//  trait typed_TyEqv extends typed_TyVis {
//    type OTy = Ty => Boolean
//
//    def tyArr = (tyT1,tyT2) => {
//      case TyArr(tyS1,tyS2) => visitTy(tyT1)(tyS1) && visitTy(tyT2)(tyS2)
//      case _ => false
//    }
//  }
//
//  @visitor
//  trait typed_TmVis[Ty] {
//    type Tm
//    def tmVar: (Int, Int) => Tm
//    def tmAbs: (String, Ty, Tm) => Tm
//    def tmApp: (Tm, Tm) => Tm
//  }
//
//trait typed_PrintTy extends typed_TyVis {
//  type OTy = String
//  override def tyArr = (t1,t2) => s"(${visitTy(t1)} -> ${visitTy(t2)})"
//}
//
//
//trait typed_Typeof[Ty] extends typed_TmVis[Ty] {
//  type OTm = Ty
//  val tyEqv: (Ty,Ty) => Boolean
//  val ty: typed_TyVis.Fact[Ty]
////  type OTm = Context[Bind] => Ty
////  def tmVar = (i, _) => ctx => ctx.getType(i)
////  def tmAbs = (v, tyT1, t) => {
////    val ctx1 = ctx.addBinding(v, VarBind)
////  }
//  def tmApp = (t1,t2) => {
//    val (ty1,ty2) = (visitTm(t1), visitTm(t2))
//    ty1 match {
//      case ty.TyArr(ty11,ty12) => if (tyEqv(ty2,ty11)) ty12 else throw new Exception("parameter mismatch")
//      case _ => throw new Exception("arrow type expected")
//    }
//  }
//}

