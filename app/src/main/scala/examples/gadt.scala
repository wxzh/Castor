package examples

@family trait GArith {
  @adt trait Tm[A] {
    case object TmZero extends Tm[Int]
    case class TmSucc(t: Tm[Int]) extends Tm[Int]
    case class TmPred(t: Tm[Int]) extends Tm[Int]
    case object TmTrue extends Tm[Boolean]
    case object TmFalse extends Tm[Boolean]
    case class TmIf[A](t1: Tm[Boolean], t2: Tm[A], t3: Tm[A])
      extends Tm[A]
    case class TmIsZero(t: Tm[Int]) extends Tm[Boolean]
  }
}

@family @adts(Tm) trait EvalGArith extends GArith {
  @visit(Tm) trait Eval {
    type OTm[A] = A
    def tmZero = 0
    def tmSucc = x => this(x.t) + 1
    def tmPred = x => this(x.t) - 1
    def tmTrue = true
    def tmFalse = false
    def tmIf[A] = x => if (this(x.t1)) this(x.t2) else this(x.t3)
    def tmIsZero = x => this(x.t) == 0
  }
}

@family @adts(Tm) trait Eval1GArith extends GArith {
  def nv[A](t: Tm[A]): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => nv(t1)
    case _ => false
  }

  @default(Tm) trait Eval1 {
    type OTm[A] = Tm[A]
    def tm[A] = x => throw NoRuleApplies

    override def tmSucc = x => this(x.t)
    override def tmPred = {
      case TmPred(TmZero) => TmZero
      case TmPred(TmSucc(t)) if nv(t) => t
      case TmPred(t) => TmPred(this(t))
    }
    override def tmIf[A] = {
      case TmIf(TmTrue,t2,_) => t2
      case TmIf(TmFalse,_,t3) => t3
      case TmIf(t1,t2,t3) => TmIf(this(t1),t2,t3)
    }
    override def tmIsZero = {
      case TmIsZero(TmZero) => TmTrue
      case TmIsZero(TmSucc(t)) if nv(t) => TmFalse
      case TmIsZero(t) => TmIsZero(this(t))
    }
  }
}


@family trait HOAS extends EvalGArith {
  @adt trait Tm[A] extends super.Tm[A] {
    case class TmVar[A](x: A) extends Tm[A]
    case class TmAbs[A, B](f: Tm[A] => Tm[B]) extends Tm[A => B]
    case class TmApp[A, B](t1: Tm[A => B], t2: Tm[A]) extends Tm[B]
  }

  @visit(Tm) trait Eval extends super.Eval {
    def tmVar[A] = _.x
    def tmAbs[A, B] = x => y => this (x.f(TmVar(y)))
    def tmApp[A, B] = x => this(x.t1)(this(x.t2))
  }
}

object TestGADT extends App {
  import HOAS._
  println(eval(TmSucc(TmZero)))   // 1
  println(eval(TmIsZero(TmZero))) // true

  val t = TmApp(TmAbs((t: Tm[Int]) => TmIsZero(TmPred(t))), TmZero)
  println(eval(t)) // false
}

@family trait HierarchicalArith {
  @adt trait Tm {
    trait ATm
    trait AppTm { val t: Tm }
    trait ANatTm extends ATm
    trait AppNatTm extends AppTm
    case object TmZero extends ANatTm
    case class TmSucc(t: Tm) extends AppNatTm
    case class TmPred(t: Tm) extends AppNatTm
    trait ABoolTm extends ATm
    trait AppBoolTm extends AppTm
    case object TmTrue extends ABoolTm
    case object TmFalse extends ABoolTm
    case class TmIf(t1: Tm, t2: Tm, t3: Tm)
    case class TmIsZero(t: Tm) extends AppBoolTm
  }

  @adt trait Ty {
    case object TyNat
    case object TyBool
  }

  @default(Tm) trait Typeof {
    type OTm = Option[Ty]
    override def aNatTm = _ => Some(TyNat)
    override def appNatTm = x => this(x.t) match {
      case ty@Some(TyNat) => ty
      case _ => None
    }
    override def aBoolTm = _ => Some(TyBool)
    override def tmIf = x => (this(x.t1),this(x.t2),this(x.t3)) match {
      case (Some(TyBool),o2,o3) if o2==o3 => o2
      case _ => None
    }
    override def tmIsZero = x => this(x.t) match {
      case Some(TyNat) => Some(TyBool)
      case _ => None
    }
    def tm = _ => None
  }
}

object TestHierarchicalArith extends App {
  import HierarchicalArith._
  println(typeof(TmSucc(TmZero)))
}