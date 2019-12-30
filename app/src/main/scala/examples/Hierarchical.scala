package examples

@family trait Hierarchical {
  @adt trait Tm {
    trait TmNullary
    trait TmUnary { val t: Tm }
    trait TmTernary { val t1, t2, t3: Tm }
    trait TmNat extends TmNullary
    trait TmBool extends TmNullary
    trait TmNat2Nat extends TmUnary
    trait TmNat2Bool extends TmUnary
    case object TmZero extends TmNat
    case class TmSucc(t: Tm) extends TmNat2Nat
    case class TmPred(t: Tm) extends TmNat2Nat
    case object TmTrue extends TmBool
    case object TmFalse extends TmBool
    case class TmIf(t1: Tm, t2: Tm, t3: Tm) extends TmTernary
    case class TmIsZero(t: Tm) extends TmNat2Bool
  }

  @visit(Tm) trait Print {
    type OTm = String
    def tmZero = "0"
    def tmUnary(x: TmUnary, op: String) = "(" + op + " " + this(x.t) + ")"
    def tmSucc = tmUnary(_,"succ")
    def tmPred = tmUnary(_,"pred")
    def tmTrue = "true"
    def tmFalse = "false"
    def tmIf = x =>
      "(if " + this(x.t1) + " " + this(x.t2) + " " + this(x.t3) + ")"
    def tmIsZero = tmUnary(_,"iszero")
  }
//  trait TmDefault2 extends TmVisit { _: TmV =>
//    def tm: Tm => OTm
//    def tmNullary = (x: TmNullary) => tm(x)
//    def tmUnary = (x: TmUnary) => tm(x)
//    def tmTernary = (x: TmTernary) => tm(x)
//    def tmNat = (x: TmNat) => tmNullary(x)
//    def tmBool = (x: TmBool) => tmNullary(x)
//    def tmNat2Nat = (x: TmNat2Nat) => tmUnary(x)
//    def tmNat2Bool = (x: TmNat2Bool) => tmUnary(x)
//    def tmZero = tmNat(TmZero)
//    def tmSucc = (x: TmSucc) => tmNat2Nat(x)
//    def tmPred = (x: TmPred) => tmNat2Nat(x)
//    def tmTrue = tmBool(TmTrue)
//    def tmFalse = tmBool(TmFalse)
//    def tmIf = (x: TmIf) => tmTernary(x)
//    def tmIsZero = (x: TmIsZero) => tmNat2Bool(x)
//  }

    @adt trait Ty {
      case object TyNat
      case object TyBool
    }
    @default(Tm) trait Typeof {
      type OTm = Option[Ty]
      override def tmBool = _ => Some(TyBool)
      override def tmNat = _ => Some(TyNat)
      override def tmNat2Nat = x => this(x.t) match {
        case Some(TyNat) => Some(TyNat)
        case _ => None
      }
      override def tmNat2Bool = x => this(x.t) match {
        case Some(TyNat) => Some(TyBool)
        case _ => None
      }
      override def tmIf = x => (this(x.t1), this(x.t2), this(x.t3)) match {
        case (Some(TyBool),ty1,ty2) if ty1 == ty2 => this(x.t2)
        case _ => None
      }
      def tm = _ => None
    }
}

object TestHierachical extends App {
  import Hierarchical._
  val term = TmIsZero(TmIf(TmFalse,TmZero,TmPred(TmSucc(TmZero))))
  println(print(term))
  println(typeof(term))
}
