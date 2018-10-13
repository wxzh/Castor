package examples

import java.util

import scala.collection.mutable._

// REFERENCE: https://github.com/cwi-swat/model-algebras/blob/master/src/nl/cwi/moalg/scratch/TestOAlgs.java

/*
// Generate unapply()?
trait BaseExp {
  @adt trait Exp
  @variant(Exp) class Lit(val v: Boolean)
  @variant(Exp) class And(val l: Exp, val r: Exp)
  @variant(Exp) class Not(val e: Exp)

  @visit(Exp) trait Eval {
    def lit = e => e.v
    def and = e => this(e.l) && this(e.r)
    def not = e => !this(e.e)
  }
}
*/
trait BaseExp {
  type ExpV[O] <: ExpVisit[O]

  trait Exp {
    def accept[O](v: ExpV[O]): O
  }

  class Lit(val v: Boolean) extends Exp {
    def accept[O](v: ExpV[O]) = v.lit(this)
  }
  class And(val l: Exp, val r: Exp) extends Exp {
    def accept[O](v: ExpV[O]) = v.and(this)
  }
  class Not(val e: Exp) extends Exp {
    def accept[O](v: ExpV[O]) = v.not(this)
  }

  trait ExpVisit[O] { _: ExpV[O] =>
    def lit: Lit => O
    def and: And => O
    def not: Not => O
    def apply(e: Exp) = e.accept(this)
  }

  trait Eval extends ExpVisit[Boolean] { _: ExpV[Boolean] =>
    def lit = e => e.v
    def and = e => this(e.l) && this(e.r)
    def not = e => !this(e.e)
  }
  val eval: Eval
}

trait OrExp extends BaseExp {
  type ExpV[O] <: ExpVisit[O]
  case class Or(l: Exp, r: Exp) extends Exp {
    def accept[O](v: ExpV[O]) = v.or(this)
  }
  trait ExpVisit[O] extends super.ExpVisit[O] { _: ExpV[O] =>
    def or: Or => O
  }

  trait Eval extends ExpVisit[Boolean] with super.Eval { _: ExpV[Boolean] =>
    def or = {
      case Or(l,r) => this(l) || this(r)
    }
  }
}

// IMAGINARY IMPLEMENTATION WITHOUT BOILERPLATE
/*
trait FSM {
  @adt class Machine(var states: MutableList[State])
  @adt class State(val name: String, var trans: MutableList[Trans] = MutableList.empty)
  @adt class Trans(val event: String, val to: State, val from: State = null)

  type INext = String => Option[State]
  @visit(Machine,State,Trans) trait Exec {
    type OMachine = INext
    type OState = INext
    type OTrans = INext
    def machine = m => ev =>
      m.states.flatMap(state => this(state)(ev)).headOption
    def state = s => ev =>
      s.trans.flatMap(tran => this(tran)(ev)).headOption
    def trans = t => ev =>
      if (ev == t.event) Some(t.to) else None
  }

  @visit(Machine,State,Trans) trait FSM2String {
    type OMachine = String
    type OState = String
    type OTrans = String
    def machine = _.states.foldLeft ("") { (acc,s) => acc + this(s) + "\n" }
    def state = s => {
      s.trans.foldLeft (s.name + ":\n") { (acc,t) => acc + this(t) }
    }
    def trans = t => t.event + "=>" + t.to.name
  }
}
*/

// hierachical class hierarchy
trait FSM2 {
  type MachineV[O] <: MachineVisit[O]
  type StateV[O] <: StateVisit[O]
  type TransV[O] <: TransVisit[O]

  class Machine(var states: MutableList[State] = MutableList.empty) {
    def accept[O](v: MachineV[O]) = v.machine(this)
  }
  class State(val name: String, var trans: MutableList[Trans] = MutableList.empty) {
    def accept[O](v: StateV[O]) = v.state(this)
  }
  class Trans(val event: String, val to: State, val from: State = null) {
    def accept[O](v: TransV[O]) = v.trans(this)
  }

  trait MachineVisit[O] { _: MachineV[O] =>
    def machine: Machine => O
    def apply(v: Machine) = v.accept(this)
  }
  trait StateVisit[O] { _: StateV[O] =>
    def state: State => O
    def apply(v: State) = v.accept(this)
  }
  trait TransVisit[O] { _: TransV[O] =>
    def trans: Trans => O
    def apply(v: Trans) = v.accept(this)
  }

  type INext = String => Option[State]
  trait Exec extends MachineVisit[INext] with StateVisit[INext] with TransVisit[INext] { _: MachineV[INext] with StateV[INext] with TransV[INext] =>
    def machine = m => ev =>
      m.states.flatMap(state => this(state)(ev)).headOption
    def state = s => ev =>
      s.trans.flatMap(tran => this(tran)(ev)).headOption
    def trans = t => ev =>
      if (ev == t.event) Some(t.to) else None
  }

  trait FSM2String extends MachineVisit[String] with StateVisit[String] with TransVisit[String] { _: MachineV[String] with StateV[String] with TransV[String] =>
    def machine = _.states.foldLeft ("") { _ + this(_) + "\n" }
    def state = s => s.trans.foldLeft (s.name + ":\n") { _ + this(_) }
    def trans = t => t.event + "=>" + t.to.name
  }
}

trait GuardedFSM extends FSM2 with OrExp {
  type TransV[O] <: TransVisit[O]
  class Guarded(val e: Exp, event: String, from: State, to: State) extends Trans(event,from,to) {
    override def accept[O](v: TransV[O]) = v.guarded(this)
  }
  trait TransVisit[O] extends super.TransVisit[O] { _: TransV[O] =>
    def guarded: Guarded => O
  }
  trait Exec extends TransVisit[INext] with super.Exec { _: TransV[INext] with MachineV[INext] with StateV[INext] =>
    def guarded = t => ev =>
      if (ev == t.event && eval(t.e)) Some(t.to) else None
  }
}

trait Graph {
  class Graph(var nodes: Set[Node] = HashSet.empty, var edges: Set[Edge] = HashSet.empty) {
    def accept[O](v: GraphV[O]) = v.graph(this)
  }
  class Node(val id: String, var out: Set[Edge] = HashSet.empty, var in: Set[Edge] = HashSet.empty) {
    def accept[O](v: NodeV[O]) = v.node(this)
  }
  class Edge(var from: Node, var to: Node, var label: String) {
    def accept[O](v: EdgeV[O]) = v.edge(this)
  }

  type GraphV[O] <: GraphVisit[O]
  type NodeV[O] <: NodeVisit[O]
  type EdgeV[O] <: EdgeVisit[O]

  trait GraphVisit[O] { _: GraphV[O] =>
    def graph: Graph => O
    def apply(g: Graph) = g.accept(this)
  }
  trait NodeVisit[O] { _: NodeV[O] =>
    def node: Node => O
    def apply(g: Node) = g.accept(this)
  }
  trait EdgeVisit[O] { _: EdgeV[O] =>
    def edge: Edge => O
    def apply(g: Edge) = g.accept(this)
  }
}

trait FSM2Graph extends FSM2 with Graph {
  trait FSM2Graph extends MachineVisit[Graph] with StateVisit[Node]  with TransVisit[Edge]  {
    _: MachineV[Graph] with StateV[Node]  with TransV[Edge] =>

    var memo = HashMap.empty[State,Node]

    def machine = m => {
      memo.clear
      val g = new Graph
      m.states.foreach { s =>
        val n = this(s)
        g.nodes += n
        g.edges ++= n.in
        g.edges ++= n.out
      }
      g
    }

    def state = s => {
      if (memo.contains(s))
        memo(s)
      else {
        val n = new Node(s.name)
        memo.put(s, n)
        s.trans.foreach { t =>
          val e = this (t)
          e.from = n
          n.out += e
        }
        n
      }
    }

    def trans = t => {
      val e = new Edge(null, this(t.to), t.event)
      this(t.to).in += e
      e
    }

  }
}

trait MemoFSM extends FSM2 {
  // overriding apply()

  def memoizing[I,O](memo: util.IdentityHashMap[I,O], arg: I, init: O, f: O => O): Unit = {
    if (!memo.containsKey(arg))
      memo.put(arg,init)
    else f(memo.get(arg))
  }

  trait MachineVisit[O] extends super.MachineVisit[O] { _: MachineV[O] =>
    val machineMemo = new util.IdentityHashMap[Machine,O]()
    override def apply(m: Machine) = {
      if (machineMemo.containsKey(m))
        machineMemo.get(m)
      else
        m.accept(this)
    }
  }
}


object FSM2Graph extends FSM2Graph {
  type MachineV[O] = MachineVisit[O]
  type StateV[O] = StateVisit[O]
  type TransV[O] = TransVisit[O]

  object exec extends Exec
  object fsm2string extends FSM2String
  object fsm2graph extends FSM2Graph
}


object TestFSM extends App {

  import FSM2Graph._

  val doors = new Machine
  val closed = new State("closed")
  val opened = new State("opened")
  closed.trans += new Trans("open", opened)
  opened.trans += new Trans("close", closed)
  doors.states ++= Seq(closed,opened)

  println(fsm2string(doors))
  Seq("open", "close", "open").foreach { ev =>
    println(exec(doors)(ev).map(fsm2string(_)))
  }
  println(fsm2graph(doors))
}

//@family trait BoolExp {
//  @adt trait Exp {
//    def Lit: Boolean => Exp
//    def And: (Exp, Exp) => Exp
//    def Not: Exp => Exp
//  }
//
//  @visit(Exp) trait Eval {
//    type OExp = Boolean
//    def lit = b => b
//    def and = (e1,e2) => this(e1) && this(e2)
//    def not = e => !this(e)
//  }
//}
//
//@family trait Or extends BoolExp {
//  @adt trait Exp extends super.Exp {
//    def Or: (Exp, Exp) => Exp
//  }
//  @visit(Exp) trait Eval extends super.Eval {
//    def or = (e1,e2) => this(e1) || this(e2)
//  }
//}
//
//@family trait FSM {
//  @adt trait Machine {
//    def F: List[State] => Machine
//  }
//  @adt trait State {
//    def S: (String, List[Trans]) => State
//  }
//  @adt trait Trans {
//    def T: (String, State, State) => Trans
//  }
//
//  @visit(Machine) trait Exec {
//    type OMachine = String => Option[State]
//    def f = states => event =>
//      states.flatMap(execState(_)(event)).headOption
//  }
//  @visit(State) trait ExecState {
//    type OState = String => Option[State]
//    def s = (name, trans) => event =>
//      trans.flatMap(execTrans(_)(event)).headOption
//  }
//  @visit(Trans) trait ExecTrans {
//    type OTrans = String => Option[State]
//    def t = (ev, from, to) => event =>
//      if (ev == event) Some(to) else None
//  }

//  @visit(Machine) trait Print {
//    type OMatchine
//  }

//  @adt trait Graph {
//    def G: (Set[Node],Set[Edge]) => Graph
//  }
//
//  @adt trait Node {
//    def N: (String, Set[Edge], Set[Edge]) => Node
//  }
//
//  @adt trait Edge {
//    def E: (String, Node, Node) => Edge
//  }

//  @visit(Machine) trait ToGraph {
//    type OMachine = Map[State,Node] => Graph
//    def f = states => memo => {
//      states.
//    }
//  }
//
//  @visit(State) trait ToNode {
//    type OState = Map[State,Node] => Node
//    def s = (name,trans) => memo =>
//      if (memo.contains())
//  }
//}