package examples

@family trait Arith extends Nat with Bool {
  @adt trait Tm extends super[Nat].Tm with super[Bool].Tm {
    def TmIsZero: Tm => Tm
  }
  @visit(Tm) trait Eval1 extends super[Nat].Eval1
                         with super[Bool].Eval1 {
    def tmIsZero = {
      case TmZero => TmTrue
      case TmSucc(t) if nv(t) => TmFalse
      case t => TmIsZero(this(t))
    }
  }
}

object TestArith extends App {
  import Arith._
  val term = TmIsZero(TmIf(TmFalse,TmTrue,TmPred(TmSucc(TmZero))))
  println(eval1(term))               // TmIsZero(TmPred(TmSucc(TmZero)))
  println(eval1(eval1(term)))        // TmIsZero(TmZero)
  println(eval1(eval1(eval1(term)))) // TmTrue
}

@family @adts(Tm) @ops(Eval1)
trait EqArith extends Arith {
  @visit(Tm) trait Equal {
    type OTm = Tm => Boolean
    def tmZero = { case TmZero => true }
    def tmSucc = t => { case TmSucc(s) => this(t)(s) }
    def tmPred = t => { case TmPred(s) => this(t)(s) }
    def tmTrue = { case TmTrue => true }
    def tmFalse = { case TmFalse => true }
    def tmIf = (t1,t2,t3) => {
      case TmIf(s1,s2,s3) => this(t1)(s1) && this(t2)(s2) && this(t3)(s3)
    }
    def tmIsZero = t => { case TmIsZero(s) => this(t)(s) }
    override def apply(t1: Tm) = t2 => {
      try { t1.accept(this)(t2) }
      catch { case _: MatchError => false }
    }
  }
}

/*
@family @adts(Tm) @ops(Eval1)
trait EqArith extends Arith {
  @visit(Tm) trait Equal {
    type OTm = Tm => Boolean
    def tmZero = {
      case TmZero => true
      case _ => false
    }
    def tmSucc = t => {
      case TmSucc(s) => this(t)(s)
      case _ => false
    }
    def tmPred = t => {
      case TmPred(s) => this(t)(s)
      case _ => false
    }
    def tmTrue = {
      case TmTrue => true
      case _ => false
    }
    def tmFalse = {
      case TmFalse => true
      case _ => false
    }
    def tmIf = (t1,t2,t3) => {
      case TmIf(s1,s2,s3) => this(t1)(s1) && this(t2)(s2) && this(t3)(s3)
      case _ => false
    }
    def tmIsZero = t => {
      case TmIsZero(s) => this(t)(s)
      case _ => false
    }
  }
}
*/
//override def apply(t: Tm) = s => {
//try { t.accept(this)(s) }
//catch { case _: MatchError => false }
//}

object TestEqArith extends App {
  import EqArith._
  val t = TmSucc(TmZero)
  println(equal(t)(TmZero)) // false
  println(equal(t)(t))      // true
}

@family @adts(Tm) @ops(Eval1)
trait TyArith extends Arith {
  @adt trait Ty {
    def TyNat: Ty
    def TyBool: Ty
  }
  @visit(Tm) trait Typeof {
    type OTm = Option[Ty]
    def tmZero = Some(TyNat)
    def tmSucc = t => for {
      ty <- this(t)
      if (ty == TyNat)
    } yield(TyNat)
    def tmPred = tmSucc
    def tmTrue = Some(TyBool)
    def tmFalse = Some(TyBool)
    def tmIf = (t1,t2,t3) =>
//        typeof(t1) match {
//      case Some(TyBool) => (typeof(t2), typeof(t3)) match {
//        case (Some(ty2), Some(ty3)) if (ty2 == ty3) => Some(ty2)
//        case _ => None
//      }
//      case _ => None
//    }
      for {
      ty1 <- this(t1)
      if (ty1 == TyBool)
      ty2 <- this(t2)
      ty3 <- this(t3)
      if (ty2 == ty3)
    } yield(ty2)
    def tmIsZero = t => for {
      ty <- this(t)
      if (ty == TyNat)
    } yield(TyBool)
  }

  @visit(Tm) trait Eval {
    type OTm = Either[Int,Boolean]
    def tmZero = Left(0)
    def tmPred = t => eval(t) match {
      case Left(i) => Left(i-1)
      case _ => throw NoRuleApplies
    }
    def tmSucc = eval(_) match {
      case Left(i) => Left(i+1)
      case _ => throw NoRuleApplies
    }
    def tmTrue = Right(true)
    def tmFalse = Right(false)
    def tmIf = (t1,t2,t3) => eval(t1) match {
      case Right(b) => if (b) eval(t2) else eval(t3)
      case _ => throw NoRuleApplies
    }
    def tmIsZero = eval(_) match {
      case Left(0) => Right(true)
      case Left(_) => Right(false)
      case _ => throw NoRuleApplies
    }
  }

  @default(Tm) trait PtmTerm {
    type OTm = String
    def otherwise = ptmAppTerm(_)
    override def tmIf =
      "if " + this(_) + " then " + this(_) + " else " + this(_)
  }
  @default(Tm) trait PtmAppTerm {
    type OTm = String
    def otherwise = ptmATerm(_)
    override def tmPred = "pred " + ptmATerm(_)
    override def tmSucc = "succ " + ptmATerm(_)
    override def tmIsZero = "iszero " + ptmATerm(_)
  }
  @default(Tm) trait PtmATerm {
    type OTm = String
    def otherwise = "(" + ptmTerm(_) + ")"
    override def tmZero = "0"
    override def tmTrue = "true"
    override def tmFalse = "false"
  }
}

//@family @adts(Tm) @ops(Eval1)
//trait PTerm extends Term {
//  @default(Tm) trait PPTerm {
//    type OTm = String
//    def otherwise = pAppTerm(_)
//  }
//  @default(Tm) trait PAppTerm extends PTerm {
//    override def otherwise = pATerm(_)
//  }
//  @default(Tm) trait PATerm extends PTerm {
//    override def otherwise = "(" + pTerm(_) + ")"
//  }
//}
//@family @adts(Tm) @ops(Eval1)
//trait PNat extends Nat with PTerm {
//  @default(Tm) trait PAppTerm extends super.PAppTerm {
//    override def tmPred = "pred" + pATerm(_)
//    override def tmSucc = "pred" + pATerm(_)
//  }
//  @default(Tm) trait PATerm extends super.PATerm {
//    override def tmZero = "0"
//  }
//}

object Test extends App {
  import TyArith._
  val term = TmIsZero(TmIf(TmFalse,TmTrue,TmPred(TmZero)))
  println(eval1(term))
  println(eval1(eval1(term)))
  println(eval1(eval1(eval1(term))))

  println(typeof(TmSucc(TmTrue)))
  println(typeof(TmSucc(TmZero)))

  println(ptmTerm(TmSucc(TmZero))) // succ 0
  val term2 = TmIsZero(TmIf(TmFalse,TmTrue,TmPred(TmSucc(TmZero))))
  // iszero(if false then true else pred(succ 0))
  println(ptmTerm(term2))
}
