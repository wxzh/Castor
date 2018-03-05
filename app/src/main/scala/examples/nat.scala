package examples

object NoRuleApplies extends Exception

@family trait Term {
  @adt trait Tm
  @default(Tm) trait Eval1 {
    type OTm = Tm
    def otherwise = _ => throw NoRuleApplies
  }
}

@family trait Nat extends Term {
  @adt trait Tm extends super.Tm {
    def TmZero: Tm
    def TmSucc: Tm => Tm
    def TmPred: Tm => Tm
  }
  def nv(t: Tm): Boolean = t match {
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

@adts(Tm) @ops(Eval1)
@family trait Type extends Term {
  @adt trait Ty
  @visit(Tm) trait Typeof { type OTm = Option[Ty] }
}

@adts(Tm) @ops(Eval1)
@family trait TyNat extends Nat with Type {
  @adt trait Ty extends super.Ty {
    def TyNat: Ty
  }
  @visit(Tm) trait Typeof extends super.Typeof {
    def tmZero = Some(TyNat)
    def tmSucc = t => for {
      ty <- this(t)
      if (ty == TyNat)
    } yield(TyNat)
    def tmPred = t => tmSucc(t)
  }
}
