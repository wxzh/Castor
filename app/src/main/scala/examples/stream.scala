package examples

// MOTIVATION: stream fusion, e.g. filter f . filter g => filter f.g

@family trait Base {
  @adt trait Stream[A] {
    def Source[A]: Array[A] => Stream[A]
    def Map[A,B]: (A => B, Stream[A]) => Stream[B]
    def FlatMap[A,B]: (A => Stream[B], Stream[A]) => Stream[B]
    def Filter[A]: (A => Boolean, Stream[A]) => Stream[A]
  }

  trait IPull[A] {
    def next: A
    def hasNext: Boolean
  }
  @visit(Stream) trait Pull {
    type OStream[A] = IPull[A]

    def source[A] = array => new IPull[A] {
      val size = array.size
      var cursor = 0
      def hasNext = cursor != size
      def next =
        if (cursor >= size) throw new Exception
        else {
          val e = array(cursor)
          cursor += 1
          e
        }
    }

    def map[A,B] = (mapper, stream) => {
      val self = this(stream)
      new IPull[B] {
        var nextElem: Option[B] = None
        def hasNext: Boolean = {
          while (self.hasNext) {
            nextElem = Some(mapper(self.next))
            return true
          }
          false
        }
        def next =
          if (nextElem.isDefined || hasNext) {
            val temp = nextElem.get
            nextElem = None
            temp
          }
          else throw new Exception
      }
    }

    def flatMap[A,B] = (mapper, stream) => {
      val self = this(stream)
      new IPull[B] {
        var current: Option[IPull[B]] = None
        var nextElem: Option[B] = None
        def hasNext: Boolean = {
          while (true) {
            while (current.isDefined && current.get.hasNext) {
              nextElem = current.map(_.next)
              return true
            }
            if (self.hasNext)
              current = Some(rec(mapper(self.next)))
            else
              return false
          }
          false
        }
        def next =
          if (current.isDefined || hasNext) {
            val temp = nextElem.get
            nextElem = None
            temp
          }
          else throw new Exception
      }
    }

    def filter[A] = (pred, stream) => {
      val self = this(stream)
      new IPull[A] {
        var nextElem:Option[A] = None
        def hasNext: Boolean = {
          while (self.hasNext) {
            val current = self.next
            if (pred(current)) {
              nextElem = Some(current)
              return true
            }
          }
          false
        }
        def next =
          if (nextElem.isDefined || hasNext) {
            val temp = nextElem.get
            nextElem = None
            temp
          }
          else throw new Exception
      }
    }
  }

  @visit(Stream) trait Push {
    type OStream[A] = (A => Unit) => Unit

    def source[A] = array => k =>
      array.foreach(k(_))

    def map[A,B] = (mapper,stream) => k =>
      this(stream)(i => k(mapper(i)))

    def flatMap[A,B] = (mapper,stream) => k =>
      this(stream)(v => this(mapper(v))(k))

    def filter[A] = (pred,stream) => k =>
      this(stream)(i => if (pred(i)) k(i))
  }
}

// Extensions:
// take
// count reduce (consumers)
// iterate

//@family trait TakeStream extends Base {
//  @adt trait Stream[A] extends super.Stream[A] {
//    def Take[A]: (Int, Stream[A]) => Stream[A]
//  }
//}

//@family trait ExecStream extends Base {
//  @adt trait Stream[A] extends super.Stream[A] {
//    def Count[A]: Stream[A] => Long
//    def Reduce[A]: (A, (A,A) => A, Stream[A]) => A
//  }
//}

