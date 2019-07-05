package tapl.extracted

import examples._
import util.Print._
import util.Document
import util.Document._

@adts(Binding)
@ops(BindingShift, PBinding)
@family
trait Record extends Term {
  @adt trait Tm extends super.Tm {
    case class TmRecord(fields: List[(String,Tm)])
    case class TmProj(t: Tm, l: String)
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmProj = x => (onvar,c) => TmProj(this(x.t)(onvar,c),x.l)
    def tmRecord = x => (onvar,c) => TmRecord(x.fields.map {case (l,t) => (l, this(t)(onvar,c))})
  }

  @default(Tm) trait PtmTerm extends super.PtmTerm

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm {
    override def tm = t => (outer,ctx) =>
      ptmPathTerm(t)(outer,ctx)
  }

  @default(Tm) trait PtmPathTerm {
    type OTm = (Boolean, Context) => Document
    override def tmProj = x => (_,ctx) =>
      ptmATerm(x.t)(false,ctx) :: "." :: x.l
    def tm = ptmATerm(_)
  }

  @default(Tm) trait PtmATerm extends super.PtmATerm {
    override def tmRecord = x => (_,ctx) => {
      def pf(i: Int, li: String, t: Tm): Document =
        if (i.toString() == li) {
          ptmTerm(t)(false, ctx)
        } else {
          li :: "=" :: ptmTerm(t)(false, ctx)
        }
      "{" :: x.fields.zipWithIndex.map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }.
        reduceLeftOption(_ :: "," :/: _).getOrElse(empty) :: "}"
    }
  }

  @visit(Tm) trait Eval1 extends super.Eval1 {
    def tmRecord = x => ctx => {
      def evalAField(l: List[(String, Tm)]): List[(String, Tm)] = l match {
        case Nil                          => throw new NoRuleApplies(TmRecord(x.fields))
        case (l, v1) :: rest if isVal(v1) => (l, v1) :: evalAField(rest)
        case (l, t1) :: rest              => (l, this(t1)(ctx)) :: rest
      }
      TmRecord(evalAField(x.fields))
    }
    def tmProj = {
      case TmProj(v1@TmRecord(fields), l) if isVal(v1) => ctx =>
        fields.find { _._1 == l } match {
          case Some((_, ti)) => ti
          case None          => throw new NoRuleApplies(TmProj(v1,l))
        }
      case TmProj(t,l) => ctx => TmProj(this(t)(ctx), l)
    }
  }

  @default(Tm) trait IsVal extends super.IsVal {
    override def tmRecord = _.fields.forall {f => this(f._2)}
  }
}

@family
@adts(Binding,Tm)
@ops(PBinding,BindingShift,PtmTerm,PtmATerm,PtmPathTerm,PtmAppTerm,IsVal,TmMap,Eval1)
trait TyRcd extends Record with Type {
  @adt trait Ty extends super.Ty {
    case class TyRecord(fields: List[(String, Ty)])
  }

  @default(Ty) trait PtyType extends super.PtyType

  @visit(Ty) trait PtyAType extends super.PtyAType {
    def tyRecord = x => (_, ctx) => {
      def pf(i: Int, li: String, tyTi: Ty): Document =
        if (i.toString() == li) {
          ptyType(tyTi)(false, ctx)
        } else {
          g0(li :: ":" :/: ptyType(tyTi)(false, ctx))
        }

      g2("{" :: x.fields.zipWithIndex.map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }.reduceLeftOption(_ :: "," :/: _).getOrElse(empty) :: "}")
    }
  }

  @visit(Tm) trait Typeof extends super.Typeof {
    def tmRecord = x => ctx =>
      TyRecord(x.fields.map { case (li, ti) => (li, this (ti)(ctx)) })

    def tmProj = x => ctx =>
      this (x.t)(ctx) match {
        case TyRecord(fieldsTys) =>
          fieldsTys find {
            _._1 == x.l
          } match {
            case Some((_, tyi)) => tyi
            case None => throw new Exception(s"Label ${x.l} not found")
          }
        case _ => throw new Exception("Expected record type")
      }
  }

  @visit(Ty) trait Subtype extends super.Subtype {
    def tyRecord = x => {
      case TyRecord(fT) =>
        fT.forall {
          case (l, tyT) => x.fields.find {
            _._1 == l
          } match {
            case Some((_, tyS)) => this (tyS)(tyT)
            case None => false
          }
        }
      case _ => false
    }
  }

  // bug in tyeqv or subtype TyRecord(List())
  @visit(Ty) trait TyEqv extends Subtype with super.TyEqv {
    override def tyRecord = x => {
      case ty@TyRecord(fields) =>
        x.fields.length == fields.length && super.tyRecord(x)(ty)
//      case _ => false
    }
  }
}
