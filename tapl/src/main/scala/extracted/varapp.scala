package tapl.extracted

import examples._
import util.Document._
import util.Print._

@adts(Binding)
@ops(BindingShift, PBinding)
@family trait VarApp extends Binding with Term {
  @adt trait Tm extends super.Tm {
    case class TmVar(i: Int, n: Int)
    case class TmApp(t1: Tm, t2: Tm)
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmVar = x => (onvar,c) => onvar(c, x.i, x.n)
    def tmApp = x => (onvar,c) => TmApp(this(x.t1)(onvar,c), this(x.t2)(onvar,c))
  }

  def termShiftAbove(d: Int, t: Tm, c: Int) =
    tmMap(t)((c, x, n) => if (x >= c) TmVar(x + d, n + d) else TmVar(x, n + d), c)

  def termShift(d: Int, t: Tm): Tm =
    termShiftAbove(d,t,0)

  // usual substitution: [j -> s]
  def termSubst(j: Int, s: Tm, t: Tm): Tm = {
    val f = (c: Int, i: Int, cl: Int) => {
      if (i == j + c) termShift(c, s) else TmVar(i, cl)
    }
    tmMap(t)(f,0)
  }

  def termSubstTop(s: Tm, t: Tm): Tm =
    termShift(-1, termSubst(0, termShift(1, s), t))

  @default(Tm) trait PtmTerm extends super.PtmTerm

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm {
    override def tmApp = x => (_, ctx) =>
      g2(this(x.t1)(false, ctx) :/: ptmATerm(x.t2)(false, ctx))
  }

  @default(Tm) trait PtmATerm extends super.PtmATerm {
    override def tmVar = x => (_, ctx) =>
      if (ctx.length == x.n) ctx.index2Name(x.i)
      else text("[bad index: " + x.i + "/" + x.n + " in {" + ctx.l.mkString(", ") + "}]")
  }

  @default(Tm) trait IsVal extends super.IsVal

  @default(Tm) trait Eval1 extends super.Eval1
}
