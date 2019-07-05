package examples


@family trait F {
  @adt trait D[X]
}


@family trait G extends F {
  @adt trait D[X] extends super.D[X] {
    trait T extends D[Int]
    class C[X] extends D[X]
    object O extends T with D[Int]
  }
  @default(D) trait V {
    type OD[_] = Unit
    override def d[_] = _ => {}
  }
}

object Generated {

  trait F {
    trait D[X] {
      def accept(v: DV): v.OD[X]
    }

    type DV <: DVisit

    trait DVisit { _: DV =>
      type OD[X]
      def apply[X](x: D[X]) = x.accept(this)
    }

    trait DDefault extends DVisit { _: DV =>
      def d[X]: D[X] => OD[X]
    }
  }

  trait G extends F {
    trait D[X] extends super.D[X]
    trait T extends D[Int]
    class C[X] extends D[X] {
      override def accept(v: DV) = v.c(this)
    }
    object O extends T with D[Int] {
      override def accept(v: DV) = v.o
    }
    type DV <: DVisit
    trait DVisit extends super.DVisit { _: DV =>
      def c[X]: C[X] => OD[X]
      def o: OD[Int]
    }
    trait DDefault extends super.DDefault with DVisit { _: DV =>
      def t = (x: T) => d(x)
      def c[X] = (x: C[X]) => d(x)
      def o = t(O)
    }
    trait V1 extends DVisit { _: DV =>
      type OD[_] = Unit
      def c[_] = _ => {}
      def o = {}
    }
    val v1: V1
    trait V2 extends DDefault { _: DV =>
      type OD[_] = Unit
      def d[_] = _ => {}
    }
    val v2: V2
  }
  object G extends G {
    type DV = DVisit
    object v1 extends V1
    object v2 extends V2
  }
}