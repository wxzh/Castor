package examples

import java.util.IdentityHashMap

import scala.collection.mutable.ListBuffer

@family trait FSM {
  @adt trait M {
    val states = ListBuffer[S]()
    class Machine
  }
  @adt trait S {
    val trans = ListBuffer[T]()
    var name: String
    class State(var name: String)
  }
  @adt trait T {
    class Trans(var event: String, var to: S)
  }
  @visit(M,S,T) trait Print {
    type OM = String
    type OS = String
    type OT = String
    def machine = _.states.map(this(_)).mkString("\n")
    def state = s => s.trans.map(this(_)).mkString(s.name + ":\n","\n","")
    def trans = t => t.event + " -> " + t.to.name
  }
  @visit(M,S,T) trait Step {
    type OM = String => Unit
    type OS = OM
    type OT = OM
    var result: S = null
    def machine = m => ev => m.states.foreach{this(_)(ev)}
    def state = s => ev => s.trans.foreach{this(_)(ev)}
    def trans = t => ev => if (ev == t.event) result = t.to
  }
}
object TestFSM extends App {
  import FSM._

  val doors = new Machine
  val opened = new State("Opened")
  val closed = new State("Closed")
  val locked = new State("Locked")
  val open = new Trans("open",opened)
  val close = new Trans("close",closed)
  val lock = new Trans("lock",locked)
  val unlock = new Trans("unlock",closed)

  opened.trans += close
  closed.trans += (open,lock)
  locked.trans += unlock
  doors.states += (opened,closed,locked)

  //  closed.trans += new GuardedTrans("open", opened) //, TmIsZero(TmSucc(TmZero)))
  //  opened.trans += new Trans("close", closed)
  println(print(doors))
  step(doors)("open")
  println(step.result.name) // "Opened"
  step.result = null
  step(doors)("close")
  println(step.result.name) // "Closed"
}

@family @adts(S,M,Tm) @ops(Eval)
trait GuardedFSM extends FSM with HOAS {
  @adt trait T extends super[FSM].T {
    class GuardedTrans(event: String, to: S, var tm: Tm[Boolean])
      extends Trans(event,to)
  }
  @visit(M,S,T) trait Step extends super.Step {
    def guardedTrans = t => ev => {
      if (eval(t.tm)) trans(t)(ev)
    }
  }
  @visit(M,S,T) trait Print extends super.Print {
    def guardedTrans = t =>
      trans(t) + " when " + t.tm.toString
  }

  @visit(S,T) trait Reachable {
    type OS = Unit
    type OT = Unit
    val reached = collection.mutable.Set[S]()
    def state = s =>
      if (!reached.contains(s)) {
        reached += s
        s.trans.foreach(this (_))
      }
    def trans = t => this(t.to)
    def guardedTrans = t =>
      if (eval(t.tm)) this(t.to)
  }
}

object TestGuardedFSM extends App {
  import GuardedFSM._

  val doors = new Machine
  val opened = new State("Opened")
  val closed = new State("Closed")
  val locked = new State("Locked")
  val open = new Trans("open",opened)
  val close = new Trans("close",closed)
  val lock = new GuardedTrans("lock",locked,TmFalse)
  val unlock = new Trans("unlock",closed)

  opened.trans += close
  closed.trans += (open,lock)
  locked.trans += unlock
  doors.states += (opened,closed,locked)

  //  closed.trans += new GuardedTrans("open", opened) //, TmIsZero(TmSucc(TmZero)))
  //  opened.trans += new Trans("close", closed)
  println(print(doors))
  step(doors)("open")
  println(step.result.name) // "Opened"
  step.result = null
  step(doors)("close")
  println(step.result.name) // "Closed"

  reachable(open)
  println(reachable.reached.size)
  reachable.reached.clear
  lock.tm = TmTrue
  reachable(open)
  println(reachable.reached.size)
  //  println(step(doors)("close").print(_))
}
