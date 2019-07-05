package tapl.extracted

import examples._
import util.Document
import util.Document._
import util.Print._


@family
@adts(Binding)
@ops(BindingShift,PBinding,GetTypeFromBind)
trait Variant extends VarBinding with Typed {
  def termSubstTop(s: Tm, t: Tm): Tm

  @adt trait Ty extends super.Ty {
    case class TyVariant(fields: List[(String,Ty)])
  }

  @adt trait Tm extends super.Tm {
    case class TmCase(t: Tm, cases: List[(String,String,Tm)])
    case class TmTag(l: String, t: Tm, ty: Ty)
  }

  @default(Tm) trait PtmTerm extends super.PtmTerm {
    override def tmCase = x => (_,ctx) => {
      def pc(li: String, xi: String, ti: Tm): Document = {
        val (ctx1, x1) = ctx.pickFreshName(xi)
        "<" :: li :: "=" :: xi :: ">==>" :: this(ti)(false, ctx1)
      }
      g2("case " :: this(x.t)(false, ctx) :: " of" :/:
        x.cases.map { case (x, y, z) => pc(x, y, z) }.foldRight(empty: Document)(_ :/: "|" :: _))
    }
    override def tmAbs = super.tmAbs
  }

  @default(Tm) trait PtmATerm extends super.PtmATerm {
    override def tmTag = x => (outer,ctx) =>
      g2("<" :: x.l :: "=" :: ptmTerm(x.t)(false, ctx) :: ">" :/: "as " :: ptyType(x.ty)(outer,ctx))
  }

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm

  @default(Ty) trait PtyType extends super.PtyType

  @default(Ty) trait PtyArrowType extends super.PtyArrowType

  @default(Ty) trait PtyAType extends super.PtyAType {
    override def tyVariant = x => (_,ctx) => {
      def pf(i: Int, li: String, tyTi: Ty): Document =
        if (i.toString() == li) {
          ptyType(tyTi)(false,ctx)
        } else {
          li :: ":" :/: ptyType(tyTi)(false,ctx)
        }
      "<" :: x.fields.zipWithIndex.map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }.
        reduceLeftOption(_ :: "," :/: _).getOrElse(empty) :: ">"
    }
  }

  @visit(Tm) trait Eval1 extends super.Eval1 {
    def tmTag = x => ctx =>
      TmTag(x.l, this(x.t)(ctx), x.ty)
    def tmCase = {
      case TmCase(TmTag(l,v,_), bs) if isVal(v) => ctx =>
        bs find { _._1 == l } match {
          case Some((_, x, body)) => termSubstTop(v, body)
          case None => throw NoRuleApplies()
        }
      case TmCase(t,bs) => ctx =>
        TmCase(this(t)(ctx),bs)
    }
  }

  @visit(Tm) trait IsVal extends super.IsVal {
    def tmTag = x =>
      this(x.t)
    def tmCase = _ =>
      false
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmTag = x => (onvar,c) => TmTag(x.l,this(x.t)(onvar,c),x.ty)
    def tmCase = x => (onvar,c) => TmCase(this(x.t)(onvar,c), x.cases.map { case (l,x,t) => (l,x,this(t)(onvar,c+1)) })
  }

  def computeTy(ctx: Context, ty: Ty): Ty = ty

  def simplifyTy(ctx: Context, ty: Ty): Ty = ty
//    try {
//      val ty1 = computeTy(ctx, ty)
//      simplifyTy(ctx, ty1)
//    } catch {
//      case _: NoRuleApplies => ty
//    }

  @visit(Tm) trait Typeof extends super.Typeof {
    def tmTag = x => ctx =>
      simplifyTy(ctx, x.ty) match {
        case TyVariant(fieldTys) =>
          fieldTys.find { _._1 == x.l } match {
            case Some((_, tyTiExpected)) =>
              val tyTi = this(x.t)(ctx)
              if (tyTi == tyTiExpected)
                x.ty
              else
                throw new Exception("field doesn't have expected type")
            case None => throw new Exception(s"label ${x.l} not found")
          }
        case z =>
          println(z)
          throw new Exception("annotation is not a variant type")
      }

    def tmCase = x => ctx =>
      simplifyTy(ctx, this(x.t)(ctx)) match {
        case TyVariant(fieldsTys) =>
          x.cases.foreach {
            case (l,_,_) => fieldsTys.find(_._1 == l) match {
              case Some(_) =>
              case None => throw new Exception(s"label $l is not in type")
            }
          }
          val casetypes = x.cases map {
            case (li,xi,ti)  =>
              val tyTi = fieldsTys.find(_._1 == li) match {
                case Some(ty) => ty._2
                case None => throw new Exception(s"label $li is not in type")
              }
              val ctx1 = ctx.addBinding(xi, VarBind(tyTi))
              //              typeShift(-1, typeof(ti)(ctx1)) // TODO
              this(ti)(ctx1)
          }
          val tyT1 :: restTy = casetypes
          restTy.foreach { tyI =>
            if (tyI != tyT1) throw new Exception("fields do not have the same type in " +  casetypes)
          }
          tyT1
        case _ => throw new Exception("Expected variant type " + x.t)
      }
  }

  @visit(Ty) trait TyEqv extends super.TyEqv {
    def tyVariant = x => {
      case TyVariant(fields) =>
        x.fields.length == fields.length && (x.fields, fields).zipped.forall {
          (f1, f2) => (f1._1 == f2._1) && this(f1._2)(f2._2)
        }
    }
  }

  @visit(Ty) trait Subtype extends super.Subtype {
    def tyVariant = x => {
      case TyVariant(fT) =>
        x.fields.forall {
          case (li, tySi) =>
            fT.find { _._1 == li } match {
              case Some((_, tyTi)) => this(tySi)(tyTi)
              case None            => false
            }
        }
    }
  }
}

