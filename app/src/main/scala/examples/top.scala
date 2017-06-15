package examples

//@visitor
//trait top_TyVis extends typed_TyVis {
//  override def tyArr: (Ty, Ty) => Ty
//  def tyTop: Ty
//}
//
//trait top_TyEqv extends top_TyVis with typed_TyEqv {
//  override type OTy = Ty => Boolean
//  def tyTop = {
//    case TyTop => true
//  }
//}
//
//trait top_Typeof[Ty,Bind] extends typed_TmVis[Ty] with typed_Typeof[Ty] {
//  override type OTm = Ty
//  val subtype: (Ty,Ty) => Boolean
//  override def tmApp = (t1, t2) => {
//    val (ty1,ty2) = (visitTm(t1),visitTm(t2))
//    ty1 match {
//      case ty.TyArr(ty11,ty12) => if (subtype(ty2,ty11)) ty12 else throw new Exception("parameter type mismatch")
//      case _ => throw new Exception("arrow type expected")
//    }
//  }
//}
//
//trait top_PrintTy extends typed_TyVis with typed_PrintTy {
//  override type OTy = String
//  def tyTop = "top"
//}
