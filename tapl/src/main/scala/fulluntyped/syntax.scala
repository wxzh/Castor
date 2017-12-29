package tapl.fulluntyped

import tapl.arith._
import tapl.untyped._
import examples._
import util.Document
import util.Document._
import util.Print._

@adts(Binding)
@ops(BindingShift, PBinding)
@vicase
trait Record extends Term {
  @adt trait Tm extends super.Tm {
    def TmRecord: List[(String,Tm)] => Tm
    def TmProj: (Tm, String) => Tm
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmProj = (t,l) => (onvar,c) => TmProj(this(t)(onvar,c),l)
    def tmRecord = fields => (onvar,c) => TmRecord(fields.map {case (l,t) => (l, this(t)(onvar,c))})
  }

  @default(Tm) trait PtmTerm extends super.PtmTerm

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm {
    override def otherwise = t => (outer,ctx) =>
      ptmPathTerm(t)(outer,ctx)
  }

  @default(Tm) trait PtmPathTerm {
    type OTm = (Boolean, Context) => Document
    override def tmProj = (t,l) => (_,ctx) =>
      ptmATerm(t)(false,ctx) :: "." :: l
    def otherwise = ptmATerm(_)
  }

  @default(Tm) trait PtmATerm extends super.PtmATerm {
    override def tmRecord = fields => (_,ctx) => {
      def pf(i: Int, li: String, t: Tm): Document =
        if (i.toString() == li) {
          ptmTerm(t)(false, ctx)
        } else {
          li :: "=" :: ptmTerm(t)(false, ctx)
        }
      "{" :: fields.zipWithIndex.map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }.
        reduceLeftOption(_ :: "," :/: _).getOrElse(empty) :: "}"
    }
  }

  @visit(Tm) trait Eval1 extends super.Eval1 {
    def tmRecord = fields => ctx => {
      def evalAField(l: List[(String, Tm)]): List[(String, Tm)] = l match {
        case Nil                          => throw new NoRuleApplies(TmRecord(fields))
        case (l, v1) :: rest if isVal(v1) => (l, v1) :: evalAField(rest)
        case (l, t1) :: rest              => (l, this(t1)(ctx)) :: rest
      }
      TmRecord(evalAField(fields))
    }
    def tmProj = {
      case (v1@TmRecord(fields), l) if isVal(v1) => ctx =>
        fields.find { _._1 == l } match {
          case Some((_, ti)) => ti
          case None          => throw new NoRuleApplies(TmProj(v1,l))
        }
      case (t,l) => ctx => TmProj(this(t)(ctx), l)
    }
  }

  @default(Tm) trait IsVal extends super.IsVal {
    override def tmRecord = _.forall {f => this(f._2)}
  }
}

@adts(Binding)
@ops(BindingShift, PBinding)
@vicase
trait Str extends Term {
  @adt trait Tm extends super.Tm {
    def TmString: String => Tm
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmString = s => (_,_) => TmString(s)
  }

  @default(Tm) trait PtmTerm extends super.PtmTerm

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm

  @visit(Tm) trait PtmATerm extends super.PtmATerm {
    def tmString = s => (_,_) => "\"" :: s :: "\""
  }

  @visit(Tm) trait IsVal extends super.IsVal {
    def tmString = _ => true
  }

  @default(Tm) trait Eval1 extends super.Eval1
}

@adts(Binding)
@ops(BindingShift, PBinding)
@vicase
trait Let extends VarApp {
  @adt trait Tm extends super.Tm {
    def TmLet: (String,Tm,Tm) => Tm
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmLet = (x,t1,t2) => (onvar,c) => TmLet(x, this(t1)(onvar,c), this(t2)(onvar,c+1))
  }

  @visit(Tm) trait PtmTerm extends super.PtmTerm {
    def tmLet = (x, t1, t2) => (_,ctx) =>
      g0("let " :: x :: " = " :: ptmTerm(t1)(false, ctx) :/: "in" :/: ptmTerm(t2)(false, ctx.addName(x)))
  }

  @default(Tm) trait PtmATerm extends super.PtmATerm

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm

  @default(Tm) trait IsVal extends super.IsVal

  @visit(Tm) trait Eval1 extends super.Eval1 {
    def tmLet = {
      case (x, v1, t2) if isVal(v1) => _ =>
        termSubstTop(v1, t2)
      case (x, t1, t2) => ctx =>
        TmLet(x,this(t1)(ctx),t2)
    }
  }
}


@vicase
@adts(Tm)
@ops(PtmTerm,PtmATerm,PtmAppTerm,IsVal,TmMap)
trait TermAbbBind extends Binding with VarApp {
  @adt trait Binding extends super.Binding {
    def TmAbbBind: Tm => Binding
  }

  @visit(Tm) trait Eval1 extends super.Eval1 {
    override def tmVar = (x,_) => ctx =>
      ctx.getBinding(x) match {
        case TmAbbBind(t) => t
        case _ => throw NoRuleApplies()
      }
  }

  @visit(Binding) trait BindingShift extends super.BindingShift {
    def tmAbbBind = t => d =>
      TmAbbBind(termShift(d, t))
  }

  @visit(Binding) trait PBinding extends super.PBinding {
    def tmAbbBind = tm => ctx =>
      "= " :: ptm(tm,ctx)
  }

  def evalBinding(ctx: Context, bind: Binding): Binding = bind match {
    case TmAbbBind(t) =>
      TmAbbBind(eval(ctx,t))
    case b =>
      b
  }
}

@adts(Binding)
@ops(BindingShift, PBinding)
@vicase
trait FullUntyped extends Untyped with Arith with Record with Let with Str with TermAbbBind {
  @adt trait Tm extends super[Untyped].Tm with super[Arith].Tm with super[Record].Tm with super[Let].Tm with super[Str].Tm
  @visit(Tm) trait IsVal extends super[Untyped].IsVal with super[Arith].IsVal with super[Record].IsVal with super[Let].IsVal with super[Str].IsVal
  @visit(Tm) trait TmMap extends super[Untyped].TmMap with super[Arith].TmMap with super[Record].TmMap with super[Let].TmMap with super[Str].TmMap
  @visit(Tm) trait PtmTerm extends super[Untyped].PtmTerm with super[Arith].PtmTerm with super[Record].PtmTerm with super[Let].PtmTerm with super[Str].PtmTerm
  @visit(Tm) trait PtmAppTerm extends super[Untyped].PtmAppTerm with super[Arith].PtmAppTerm with super[Record].PtmAppTerm with super[Let].PtmAppTerm with super[Str].PtmAppTerm
  @visit(Tm) trait PtmATerm extends super[Untyped].PtmATerm with super[Arith].PtmATerm with super[Record].PtmATerm with super[Let].PtmATerm with super[Str].PtmATerm
  @default(Tm) trait PtmPathTerm extends super[Record].PtmPathTerm
  @visit(Tm) trait Eval1 extends super[Untyped].Eval1 with super[Arith].Eval1 with super[Record].Eval1 with super[Let].Eval1 with super[Str].Eval1 with super[TermAbbBind].Eval1
}

