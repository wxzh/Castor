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

@family
@adts(Binding,Tm)
@ops(PBinding,BindingShift,PtmTerm,PtmATerm,PtmPathTerm,PtmAppTerm,IsVal,TmMap,Eval1)
trait TyRcd extends Record with Type {
  @adt trait Ty extends super.Ty {
    def TyRecord: List[(String, Ty)] => Ty
  }

  //  trait rcd_TyMap extends rcd_TyVisitor with TyMap {
  //    def tyRecord = fieldTys => c => TyRecord(fieldTys.map { case (l, ty) => (l, this (ty)(c)) })
  //  }

  @default(Ty) trait PtyType extends super.PtyType

  @visit(Ty) trait PtyAType extends super.PtyAType {
    def tyRecord = fields => (_, ctx) => {
      def pf(i: Int, li: String, tyTi: Ty): Document =
        if (i.toString() == li) {
          ptyType(tyTi)(false, ctx)
        } else {
          g0(li :: ":" :/: ptyType(tyTi)(false, ctx))
        }

      g2("{" :: fields.zipWithIndex.map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }.reduceLeftOption(_ :: "," :/: _).getOrElse(empty) :: "}")
    }
  }

  @visit(Tm) trait Typeof extends super.Typeof {
    def tmRecord = fields => ctx =>
      TyRecord(fields.map { case (li, ti) => (li, this (ti)(ctx)) })

    def tmProj = (t, l) => ctx =>
      this (t)(ctx) match {
        case TyRecord(fieldsTys) =>
          fieldsTys find {
            _._1 == l
          } match {
            case Some((_, tyi)) => tyi
            case None => throw new Exception(s"Label $l not found")
          }
        case _ => throw new Exception("Expected record type")
      }
  }

  @visit(Ty) trait Subtype extends super.Subtype {
    def tyRecord = fS => {
      case TyRecord(fT) =>
        fT.forall {
          case (l, tyT) => fS.find {
            _._1 == l
          } match {
            case Some((_, tyS)) => this (tyS)(tyT)
            case None => false
          }
        }
      case _ => false
    }
  }

  @visit(Ty) trait TyEqv extends Subtype with super.TyEqv {
    override def tyRecord = fS => {
      case ty@TyRecord(fT) =>
        fS.length == fT.length && super.tyRecord(fS)(ty)
    }
  }

}
