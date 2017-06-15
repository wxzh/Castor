package test

import examples._


@visitor
trait TmVis {
  type Tm
  def lit: Int => Tm
  def add: (Tm, Tm) => Tm
}

//@visitor
//trait Typed {
//  type Ty
//  type Tm
//  def tmAbs: (String
//}

//@visitor
//trait ExtTmVis extends TmVis {
//  override type Tm
//  def tru: Tm
//
//  override def lit: Int => Tm
//  override def add: (Tm, Tm) => Tm
//}
