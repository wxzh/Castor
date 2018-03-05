package tapl.extracted

import examples._
import util.Document._
import util.Print._

@adts(Binding)
@ops(BindingShift, PBinding)
@family trait VarApp extends Binding with Term {
  @adt trait Tm extends super.Tm {
    def TmVar: (Int, Int) => Tm
    def TmApp: (Tm, Tm) => Tm
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmVar = (x, n) => (onvar,c) => onvar(c, x, n)
    def tmApp = (t1, t2) => (onvar,c) => TmApp(this (t1)(onvar,c), this (t2)(onvar,c))
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
    override def tmApp = (t1, t2) => (_, ctx) =>
      g2(this (t1)(false, ctx) :/: ptmATerm(t2)(false, ctx))
  }

  @default(Tm) trait PtmATerm extends super.PtmATerm {
    override def tmVar = (x, n) => (_, ctx) =>
      if (ctx.length == n) ctx.index2Name(x)
      else text("[bad index: " + x + "/" + n + " in {" + ctx.l.mkString(", ") + "}]")
  }

  @default(Tm) trait IsVal extends super.IsVal

  @default(Tm) trait Eval1 extends super.Eval1
}
