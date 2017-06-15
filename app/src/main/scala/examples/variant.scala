package examples

//  @visitor trait TmVis[Ty] {
//    type Tm
//    def tmCase: (Tm,List[(String,String,Tm)]) => Tm
//    def tmTag: (String,Tm,Ty) => Tm
//  }
//
//  trait Eval1[Ty] extends TmVis[Ty] {
//    type OTm = Tm
////    def tmCase = {
////      case TmTag(l,t,_),bs) if isVal()=>
////      case (t,bs) =>
////        val (t1,store
////    }
//    def tmTag = (l,t,ty) => TmTag(l,visitTm(t),ty)
//  }
//
//  @visitor trait TyVis {
//    type Ty
//    def tyVariant: List[(String,Ty)] => Ty
//  }
//
//  trait TyEqv extends TyVis {
//    type OTy = Ty => Boolean
//    def tyVariant = fields1 => {
//      case TyVariant(fields2) =>
//        fields1.length == fields2.length && (fields1,fields2).zipped.forall {
//          (f1,f2) => (f1._1 == f2._1) && visitTy(f1._2)(f2._2)
//        }
//    }
//  }
//
//  trait Subtype extends TyVis {
//    type OTy = Ty => Boolean
//    def tyVariant = fS => {
//      case TyVariant(fT) => fS.length == fT.length && fS.forall{ case (l1,ty1) =>
//        fT.exists{ case (l2,ty2) => l1 == l2 && apply(ty1,ty2) }}
//    }
//    def apply(ty1: Ty, ty2: Ty) = visitTy(ty1)(ty2)
//  }
//}

