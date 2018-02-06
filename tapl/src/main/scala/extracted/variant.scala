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
    def TyVariant: List[(String,Ty)] => Ty
  }

  @adt trait Tm extends super.Tm {
    def TmCase: (Tm, List[(String,String,Tm)]) => Tm
    def TmTag: (String, Tm, Ty) => Tm
  }

  @default(Tm) trait PtmTerm extends super.PtmTerm {
    override def tmCase = (t, cases) => (_,ctx) => {
      def pc(li: String, xi: String, ti: Tm): Document = {
        val (ctx1, x1) = ctx.pickFreshName(xi)
        "<" :: li :: "=" :: xi :: ">==>" :: this(ti)(false, ctx1)
      }
      g2("case " :: this(t)(false, ctx) :: " of" :/:
        cases.map { case (x, y, z) => pc(x, y, z) }.foldRight(empty: Document)(_ :/: "|" :: _))
    }
    override def tmAbs = super.tmAbs
  }

  @default(Tm) trait PtmATerm extends super.PtmATerm {
    override def tmTag = (l, t, ty) => (outer,ctx) =>
      g2("<" :: l :: "=" :: ptmTerm(t)(false, ctx) :: ">" :/: "as " :: ptyType(ty)(outer,ctx))
  }

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm

  @default(Ty) trait PtyType extends super.PtyType

  @default(Ty) trait PtyArrowType extends super.PtyArrowType

  @default(Ty) trait PtyAType extends super.PtyAType {
    override def tyVariant = fields => (_,ctx) => {
      def pf(i: Int, li: String, tyTi: Ty): Document =
        if (i.toString() == li) {
          ptyType(tyTi)(false,ctx)
        } else {
          li :: ":" :/: ptyType(tyTi)(false,ctx)
        }
      "<" :: fields.zipWithIndex.map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }.
        reduceLeftOption(_ :: "," :/: _).getOrElse(empty) :: ">"
    }
  }

  @visit(Tm) trait Eval1 extends super.Eval1 {
    def tmTag = (l,t,ty) => ctx =>
      TmTag(l, this(t)(ctx), ty)
    def tmCase = {
      case (TmTag(l,v,_), bs) if isVal(v) => ctx =>
        bs find { _._1 == l } match {
          case Some((_, x, body)) => termSubstTop(v, body)
          case None => throw NoRuleApplies()
        }
      case (t,bs) => ctx =>
        TmCase(this(t)(ctx),bs)
    }
  }

  @default(Tm) trait IsVal extends super.IsVal {
    override def tmTag = (_,t,_) =>
      this(t)

    override def tmAbs = super.tmAbs
  }

//  trait variant_TyMap extends variant_TyDefault with typed_TyMap {_: TyV =>
//    override def tyVariant = fieldsTys => c => TyVariant(fieldsTys.map { case (l,ty) => (l,this(ty)(c)) })
//  }

  @visit(Tm) trait TmMap extends super.TmMap {
//    val onType: (Int, Ty) => Ty
    def tmTag = (l,t,ty) => (onvar,c) => TmTag(l,this(t)(onvar,c),ty)
    def tmCase = (t,cases) => (onvar,c) => TmCase(this(t)(onvar,c), cases.map { case (l,x,t) => (l,x,this(t)(onvar,c+1)) })
  }

  def computeTy(ctx: Context, ty: Ty): Ty = ty

  def simplifyTy(ctx: Context, ty: Ty): Ty =
    try {
      val ty1 = computeTy(ctx, ty)
      simplifyTy(ctx, ty1)
    } catch {
      case _: NoRuleApplies => ty
    }

  @visit(Tm) trait Typeof extends super.Typeof {
    def tmTag = (li, ti, tyT) => ctx =>
      simplifyTy(ctx, tyT) match {
        case TyVariant(fieldTys) =>
          fieldTys.find { _._1 == li } match {
            case Some((_, tyTiExpected)) =>
              val tyTi = this(ti)(ctx)
              if (tyTi == tyTiExpected)
                tyT
              else
                throw new Exception("field doesn't have expected type")
            case None => throw new Exception(s"label $li not found")
          }
        case z =>
          println(z)
          throw new Exception("annotation is not a variant type")
      }

    def tmCase = (t,cases) => ctx =>
      simplifyTy(ctx, this(t)(ctx)) match {
        case TyVariant(fieldsTys) =>
          cases.foreach {
            case (l,_,_) => fieldsTys.find(_._1 == l) match {
              case Some(_) =>
              case None => throw new Exception(s"label $l is not in type")
            }
          }
          val casetypes = cases map {
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
        case _ => throw new Exception("Expected variant type " + t)
      }
  }

  @visit(Ty) trait TyEqv extends super.TyEqv {
    def tyVariant = fields1 => {
      case TyVariant(fields2) =>
        fields1.length == fields2.length && (fields1, fields2).zipped.forall {
          (f1, f2) => (f1._1 == f2._1) && this(f1._2)(f2._2)
        }
    }
  }

  @visit(Ty) trait Subtype extends super.Subtype {
    def tyVariant = fS => {
      case TyVariant(fT) =>
        fS.forall {
          case (li, tySi) =>
            fT.find { _._1 == li } match {
              case Some((_, tyTi)) => this(tySi)(tyTi)
              case None            => false
            }
        }
    }
  }
}

