package examples

// parametric ADT
//@family trait List {
//  @adt trait List[T] {
//    def Nil: List[T]
//    def Cons: (T, List[T]) => List[T]
//  }
//}

@family trait GTerm {
  @adt trait Tm[A]
  @visit(Tm) trait Eval { type OTm[A] = A }
  @visit(Tm) trait Eval1 {
    type OTm[A] = Tm[A]
    def otherwise[A] = (_: Tm[A]) => throw NoRuleApplies
  }
}
@family trait GNat extends GTerm {
  @adt trait Tm[A] extends super.Tm[A] {
    def TmZero: Tm[Int]
    def TmSucc: Tm[Int] => Tm[Int]
    def TmPred: Tm[Int] => Tm[Int]
  }
  @visit(Tm) trait Eval extends super.Eval {
    def tmZero = 0
    def tmSucc = this (_) + 1
    def tmPred = this (_) - 1
  }
  def nv[A](t: Tm[A]): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => nv(t1)
    case _ => false
  }
  @default(Tm) trait Eval1 extends super.Eval1 {
    override def tmSucc = t => TmSucc(this(t))
    override def tmPred = {
      case TmZero => TmZero
      case TmSucc(t) if nv(t) => t
      case t => TmPred(this(t))
    }
  }
}
@family trait GBool extends GTerm {
  @adt trait Tm[A] extends super.Tm[A] {
    def TmTrue: Tm[Boolean]
    def TmFalse: Tm[Boolean]
    def TmIf[A]: (Tm[Boolean], Tm[A], Tm[A]) => Tm[A]
  }
  @visit(Tm) trait Eval extends super.Eval {
    def tmTrue = true
    def tmFalse = false
    def tmIf[_] = (t1,t2,t3) => if (this(t1)) this(t2) else this(t3)
  }
  @default(Tm) trait Eval1 extends super.Eval1 {
    override def tmIf[_] = {
      case (TmTrue,t2,_) => t2
      case (TmFalse,_,t3) => t3
      case (t1,t2,t3) => TmIf(this(t1),t2,t3)
    }
  }
}
@family trait GArith extends GNat with GBool {
  @adt trait Tm[A] extends super[GNat].Tm[A] with super[GBool].Tm[A] {
    def TmIsZero: Tm[Int] => Tm[Boolean]
  }
  @visit(Tm) trait Eval extends super[GNat].Eval with super[GBool].Eval {
    def tmIsZero = this(_) == 0
  }
  @visit(Tm) trait Eval1 extends super[GNat].Eval1 with super[GBool].Eval1 {
    def tmIsZero = {
      case TmZero => TmTrue
      case TmSucc(t) if nv(t) => TmFalse
      case t => TmIsZero(this(t))
    }
  }
}
@family trait GadtArith {

  @adt trait Tm[A] {
    def TmZero: Tm[Int]

    def TmSucc: Tm[Int] => Tm[Int]

    def TmPred: Tm[Int] => Tm[Int]

    def TmTrue: Tm[Boolean]

    def TmFalse: Tm[Boolean]

    def TmIf[A]: (Tm[Boolean], Tm[A], Tm[A]) => Tm[A]

    def TmIsZero: Tm[Int] => Tm[Boolean]
  }

  @visit(Tm) trait Eval {
    type OTm[A] = A

    def tmZero = 0

    def tmSucc = this (_) + 1

    def tmPred = this (_) - 1

    def tmTrue = true

    def tmFalse = false

    def tmIf[_] = (t1, t2, t3) => if (this (t1)) this (t2) else this (t3)

    def tmIsZero = this (_) == 0
  }

  def nv[A](t: Tm[A]): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => nv(t1)
    case _ => false
  }

  @visit(Tm) trait Eval1 {
    type OTm[A] = Tm[A]

    def tmZero = TmZero

    def tmSucc = t => TmSucc(this (t))

    def tmPred = {
      case TmZero => TmZero
      case TmSucc(t) if nv(t) => t
      case t => TmPred(this (t))
    }

    def tmTrue = TmTrue

    def tmFalse = TmFalse

    def tmIf[_] = {
      case (TmTrue, t2, _) => t2
      case (TmFalse, _, t3) => t3
      case (t1, t2, t3) => TmIf(this (t1), t2, t3)
    }

    def tmIsZero = {
      case TmZero => TmTrue
      case TmSucc(t) if nv(t) => TmFalse
      case t => TmIsZero(this (t))
    }
  }

  //  @visit(Tm) trait Transform {
  //    type OTm[A] = Tm[A]
  //    def tmZero = TmZero
  //    def tmSucc = t => TmSucc(this(t))
  //    def tmPred = t => TmPred(this(t))
  //    def tmTrue = TmTrue
  //    def tmFalse = TmFalse
  //    def tmIf[_] = (t1,t2,t3) => TmIf(this(t1),this(t2),this(t3))
  //    def tmIsZero = t => TmIsZero(this(t))
  //  }
  //
  ////  def optimize[A](t: Tm[A]): Tm[A] = optimize(t) match {
  ////    case TmIsZero(TmZero) => TmTrue
  ////    case TmPred(t1) => TmPred(optimize(t))
  ////    case s => s
  ////  }
  //  // rule: iszero(zero) = true
  //  @visit(Tm) trait Optimize {
  //    type OTm[A] = Tm[A]
  //    def tmIsZero = t => this(t) match {
  //      case TmZero => TmTrue // interesting case!
  //      case s => TmIsZero(s)
  //    }
  //    def tmSucc = t => TmSucc(this(t))
  //    def tmPred = t => TmPred(this(t))
  //    def tmZero = TmZero
  //    def tmTrue = TmTrue
  //    def tmFalse = TmFalse
  //    def tmIf[_] = (t1,t2,t3) => TmIf(this(t1),this(t2),this(t3))
  //  }
  //
  ////  @visit(Tm)
  ////  trait Optimize2 {
  ////    type OTm[A] = (Tm[A],Boolean)
  ////    def tmIsZero = t => b => this(t)
  ////    def tmSucc = t => TmSucc(this(t))
  ////    def tmPred = t => TmPred(this(t))
  ////    def tmZero = (TmZero,true)
  ////    def tmTrue = (TmTrue,false)
  ////    def tmFalse = (TmFalse,false)
  ////    def tmIf[_] = (t1,t2,t3) => (TmIf(this(t1),this(t2),this(t3))
  ////  }
  //
  //  @visit(Tm) trait Print {
  //    type OTm[A] = Int => String
  //    def tmZero = _ => "0"
  //    def tmSucc = t => i => "(succ " + this(t)(i) + ")"
  //    def tmPred = t => i => "(pred " + this(t)(i) + ")"
  //    def tmTrue = _ => "true"
  //    def tmFalse = _ => "false"
  //    def tmIf[_] = (t1,t2,t3) => i => "if" + this(t1)(i) + " then " + this(t2)(i) + " else " + this(t3)(i)
  //    def tmIsZero = t => i => "(iszero " + this(t)(i)
  //  }
  //}
  //
  //@family trait Lambda extends GadtArith {
  //  @adt trait Tm[A] extends super.Tm[A] {
  //    def TmVar[A]: A => Tm[A]
  //    def TmAbs[A,B]: (Tm[A]=>Tm[B]) => Tm[A=>B]
  //    def TmApp[A,B]: (Tm[A=>B], Tm[A]) => Tm[B]
  //  }
  //  @visit(Tm) trait Eval extends super.Eval {
  //    def tmVar[A] = x => x
  //    def tmAbs[A,B] = f => x => this(f(TmVar(x)))
  //    def tmApp[A,B] = (t1,t2) => this(t1)(this(t2))
  //  }
  //
  //  @visit(Tm) trait Print extends super.Print {
  //    def tmVar[_] = x => _ => x.toString
  //    def tmAbs[A,B] = f => i => "\\x" + i + "."
  //    // expected: Int => String
  //    // f: Tm[A] => Tm[B]
  //    def tmApp[A,B] = (t1,t2) => i => this(t1)(i) + " " + this(t2)(i)
  //  }
  //  @visit(Tm) trait Optimize extends super.Optimize {
  //    def tmVar[_] = x => TmVar(x)
  //    def tmAbs[A,B] = f => TmAbs(f)
  //    def tmApp[A,B] = (t1,t2) => TmApp(this(t1),this(t2))
  //  }
  ////  @visit(Tm) trait Transform extends super.Transform {
  ////    def tmVar[A] = x => TmVar(x)
  ////    def tmAbs[A,B] = f =>
  ////    def tmApp[A,B] = (t1,t2) => TmApp(this(t1),this(t2))
  ////  }
  //
  ////  @visit(Tm) trait Equal {
  ////    type OTm[A] = Tm[A] => Boolean
  ////    def tmZero = {
  ////      case TmZero => true
  ////    }
  ////    def tmSucc = t1 => {
  ////      case TmSucc(t2) => this(t1)(t2)
  ////    }
  ////    def tmPred = t1 => {
  ////      case TmPred(t2) => this(t1)(t2)
  ////    }
  ////    def tmTrue = {
  ////      case TmTrue => true
  ////    }
  ////    def tmFalse = {
  ////      case TmFalse => true
  ////    }
  ////    def tmIf[A] = (t1,t2,t3) => {
  ////      case TmIf(s1,s2,s3) => this(t1)(s1) && this(t2)(s2) && this(t3)(s3)
  ////    }
  ////    def tmVar[A] = x => {
  ////      case TmVar(y) => x == y
  ////    }
  ////    def tmAbs[A,B] = f => {
  ////      case TmAbs(g) => f == g
  ////    }
  ////    def tmApp[A,B] = (t1, t2) => {
  ////      case TmApp(s1,s2) => this(t1)(s1) && this(t2)(s2)
  ////    }
  ////  }
  //  //
  ////  @visit(Tm) trait EtaExpansion {
  ////    type OTm[A] = Tm[A]
  ////    def tmLit[A] = x => TmLit(x)
  ////    def tmAbs[A,B] = f => {
  ////      case TmAbs(g) => n => this(f(TmLit(n)))(g(TmLit(n)))(n+1)
  ////    }
  ////    def tmApp[A,B] = {
  ////      case (TmAbs(f),t) => f(this(t))
  ////      case (t1,t2) => TmApp(this(t1),this(t2))
  ////    }
  ////  }
  //}
}

object TestGadt extends App {
  import GArith._
  println(eval(TmIsZero(TmZero))) // true
  println(eval1(TmIsZero(TmZero))) // TmTrue
//  TmIsZero(TmTrue) // type error
//  val t = TmApp(TmAbs(t => TmSucc(t)), TmZero)
//  println(eval(t))
//  println(equal(t)(t))
//  println((TmApp(TmAbs(t => TmSucc(t)), TmZero)) == (TmApp(TmAbs(t => TmSucc(t)), TmZero)))

//  import Lambda._
//  val t = TmApp(TmAbs[Boolean,Boolean](t => TmIf(t,TmTrue,TmFalse)), TmIsZero(TmZero))
//  println(optimize(t))
}
