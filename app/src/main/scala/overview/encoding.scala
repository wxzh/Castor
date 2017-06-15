package overview

class encoding extends App {
    trait TmVis extends TmVis.Fact {
      type OTm
      def lit: Int => OTm
      def add: (Tm, Tm) => OTm
      def visitTm: Tm => OTm
    }
    object TmVis {
      trait Fact {
        type Tm
        trait LitLike {
          def apply(x1: Int): Tm
          def unapply(x: Tm): Option[Int]
        }
        trait AddLike {
          def apply(x1: Tm, x2: Tm): Tm
          def unapply(x: Tm): Option[(Tm, Tm)]
        }
        val Lit: LitLike
        val Add: AddLike
      }
      trait CFact extends Fact {
        type Tm = CTm
        object Lit extends LitLike {
          def apply(x1: Int) = new CTm {
            def accept(v: TmVis {type Tm = CTm}): v.OTm = v.lit(x1)
            override def fromLit = Some(x1)
          }
          def unapply(x: Tm) = x.fromLit
        }
        object Add extends AddLike {
          def apply(x1: Tm, x2: CTm) = new CTm {
            def accept(v: TmVis {type Tm = CTm}): v.OTm = v.add(x1, x2)
            override def fromAdd = Some(x1, x2)
          }
          def unapply(x: CTm) = x.fromAdd
        }
      }
      object Fact extends CFact
      trait CTmVis extends TmVis with CFact {
        def visitTm = _.accept(this)
      }
      trait CTm {
        def fromLit: Option[Int] = None
        def fromAdd: Option[(CTm, CTm)] = None
        def accept(v: TmVis{type Tm = CTm}): v.OTm
      }
    }

    trait ExtTmVis extends TmVis with ExtTmVis.Fact {
      def tru: OTm
    }
    object ExtTmVis {
      trait Fact extends TmVis.Fact {
        trait TruLike {
          def apply(): Tm
          def unapply(x: Tm): Boolean
        }
        val Tru: TruLike
      }
      trait CFact extends Fact {
        type Tm = CTm
        object Tru extends TruLike {
          def apply() = new CTm {
            def accept(v: ExtTmVis { type Tm = CTm }): v.OTm = v.tru
            override def fromTru = true
          }
          def unapply(x: Tm) = x.fromTru
        }
        object Lit extends LitLike {
          def apply(x1: Int) = new CTm {
            def accept(v: ExtTmVis { type Tm = CTm }): v.OTm = v.lit(x1)
            override def fromLit = Some(x1)
          }
          def unapply(x: Tm) = x.fromLit
        }
        object Add extends AddLike {
          def apply(x1: Tm, x2: Tm) = new CTm {
            def accept(v: ExtTmVis { type Tm = CTm }): v.OTm = v.add(x1, x2)
            override def fromAdd = Some(x1, x2)
          }
          def unapply(x: Tm) = x.fromAdd
        }
      }
      object Fact extends CFact
      trait CExtTmVis extends ExtTmVis with CFact { def visitTm = _.accept(this) }
      trait CTm {
        def fromTru: Boolean = false
        def fromLit: Option[Int] = None
        def fromAdd: Option[(CTm, CTm)] = None
        def accept(v: ExtTmVis { type Tm = CTm }): v.OTm
      }
    }

  import ExtTmVis.Fact._
  val e = Add(Lit(1),Tru())
}


