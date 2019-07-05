package uml

import examples._
import collection.mutable._

@family trait ExprLang {
  @adt trait Value {
    class IntegerValue(val value: Int)
    class BooleanValue(val value: Boolean)
  }

  @adt trait Variable {
    val name: String
    val initialValue: Value
    var currentValue: Value = null

    class IntegerVariable(val name: String, val initialValue: IntegerValue)
    class BooleanVariable(val name: String, val initialValue: BooleanValue)
  }

  @adt trait Expression {
    trait IntegerExpression {
      val operand1: IntegerVariable
      val operand2: IntegerVariable
    }

    trait BooleanExpression {
      val assignee: BooleanVariable
    }

    class IntegerCalculationExpression(val assignee: IntegerVariable,
                                       val operator: IntegerCalculationOperator,
                                       val operand1: IntegerVariable,
                                       val operand2: IntegerVariable) extends IntegerExpression

    class IntegerComparisonExpression( val assignee: BooleanVariable,
                                       val operator: IntegerComparisonOperator,
                                       val operand1: IntegerVariable,
                                       val operand2: IntegerVariable) extends IntegerExpression

    class BooleanBinaryExpression (val assignee: BooleanVariable,
                                   val operator: BooleanBinaryOperator,
                                   val operand1: BooleanVariable,
                                   val operand2: BooleanVariable) extends BooleanExpression

    class BooleanUnaryExpression(val assignee: BooleanVariable,
                                 val operator: BooleanUnaryOperator,
                                 val operand: BooleanVariable) extends BooleanExpression
  }

  @adt trait IntegerCalculationOperator {
    object ADD
    object SUBTRACT
  }

  @adt trait IntegerComparisonOperator {
    object SMALLER
    object SMALLER_EQUALS
    object EQUALS
    object GREATER_EQUALS
    object GREATER
  }

  @adt trait BooleanUnaryOperator {
    object NOT
  }

  @adt trait BooleanBinaryOperator {
    object AND
    object OR
  }

  @visit(IntegerCalculationOperator,IntegerComparisonOperator,BooleanUnaryOperator,BooleanBinaryOperator,Expression)
  trait Execute {
    type OExpression = Unit
    type OIntegerCalculationOperator = (Int,Int) => Int
    type OIntegerComparisonOperator = (Int,Int) => Boolean
    type OBooleanUnaryOperator = Boolean => Boolean
    type OBooleanBinaryOperator = (Boolean,Boolean) => Boolean
    def aDD = _ + _
    def sUBTRACT = _ - _
    def sMALLER = _ < _
    def sMALLER_EQUALS = _ <= _
    def eQUALS = _ == _
    def gREATER_EQUALS = _ >= _
    def gREATER = _ > _
    def nOT = !(_)
    def aND = _ && _
    def oR = _ || _


    def integerCalculationExpression = e => {
      val v1 = currentValue(e.operand1)
      val v2 = currentValue(e.operand2)

      val result = this(e.operator)(v1,v2)
      e.assignee.currentValue = new IntegerValue(result)
    }

    def integerComparisonExpression = e => {
      val v1 = currentValue(e.operand1)
      val v2 = currentValue(e.operand2)

      val result = this(e.operator)(v1,v2)
      e.assignee.currentValue = new BooleanValue(result)
    }

    def booleanBinaryExpression = e => {
      val v1 = currentValue(e.operand1)
      val v2 = currentValue(e.operand2)

      val result = this(e.operator)(v1,v2)
      e.assignee.currentValue = new BooleanValue(result)
    }

    def booleanUnaryExpression = e => {
      val v = currentValue(e.operand)

      val result = this(e.operator)(v)
      e.assignee.currentValue = new BooleanValue(result)
    }
  }

  def currentValue(variable: IntegerVariable) = variable.currentValue match {
    case v: IntegerValue => v.value
  }
  def currentValue(variable: BooleanVariable) = variable.currentValue match {
    case v: BooleanValue => v.value
  }
}

@family @adts(Expression,BooleanUnaryOperator,BooleanBinaryOperator,IntegerCalculationOperator,IntegerComparisonOperator) @ops(Execute)
trait UMLLang extends ExprLang {
  trait Activity {
    val name: String
    var nodes = ListBuffer[ActivityNode]()
    var edges = ListBuffer[ActivityEdge]()
    var locals = ListBuffer[Variable]()
    var inputs = ListBuffer[Variable]()
    var trace: Trace = null
    def initializeTrace {
     trace = new Trace
    }
    def initialize(inputValues: ListBuffer[InputValue]) {
      locals.foreach { v =>
        v.currentValue = v.initialValue
      }
      if (inputValues != null) {
        inputValues.foreach { v =>
          v.variable.currentValue = v.value
        }
      }
      nodes.foreach { _.activity = this }
    }
    def runNodes {
      nodes.foreach { node =>
        node.run
      }
    }
    def fireInitialNode {
      fireNode(getInitialNode)
    }
    def fireNode(node: ActivityNode) {
//      println("fire node " + node.name)
      val tokens = node.takeOffedTokens
      fire(node)(tokens)
      trace.executedNodes += node
    }
    def getInitialNode: InitialNode = {
      nodes.foreach { node =>
        if (node.isInstanceOf[InitialNode]) {
          return node.asInstanceOf[InitialNode]
        }
      }
      null
    }
    def getEnabledNodes = {
      val enabledNodes = ListBuffer[ActivityNode]()
      nodes.foreach { node =>
        if (isReady(node))
          enabledNodes += node
      }
      enabledNodes
    }
    def main(inputValues: ListBuffer[InputValue]) {
      initialize(inputValues)
      initializeTrace
      runActivity
    }
    def runActivity {
      runNodes
      fireInitialNode
      var enabledNodes = getEnabledNodes
      while (enabledNodes.size > 0) {
        enabledNodes.foreach { nextNode =>
          fireNode(nextNode)
          enabledNodes = getEnabledNodes
        }
      }
    }
    def terminateNodes {
      nodes.foreach {
        _.terminate
      }
    }
  }
  class ActivityEdge(val name: String, source: ActivityNode, target: ActivityNode) {
    var offers = ListBuffer[Offer]()

    def sendOffer(tokens: ListBuffer[Token]) {
      val offer = new Offer{}
      tokens.foreach { token =>
        offer.offeredTokens += token
      }
      offers += offer
    }

    def takeOfferedTokens = {
      val tokens = ListBuffer[Token]()
      offers.foreach { o =>
        tokens ++= o.offeredTokens
      }
      offers.clear
      tokens
    }
    def hasOffer: Boolean = {
      //println("# of offers " + name + ":" + offers.size)
      offers.foreach { o =>
        if (o.hasTokens) {
           //println(name + " hasOffer: true")
           return true
        }
      }
      //println(name + " hasOffer: false")
      false
    }
  }
  class ControlFlow(name: String, source: ActivityNode, target: ActivityNode, val guard: BooleanVariable)
    extends ActivityEdge(name,source,target)



  @default(ActivityNode) trait IsReady {
    type OActivityNode = Boolean

    override def initialNode = _ => false

    override def activityNode = node =>
      node.isRunning && node.hasOffers

    override def joinNode = node => {
      var ready = true
      node.incoming.foreach { edge =>
        if (!edge.hasOffer) {
          ready = false
        }
      }
      ready
    }

    override def decisionNode = node => {
      var ready = true
      node.incoming.foreach { edge =>
        if (!edge.hasOffer)
          ready = false
      }
      ready
    }
  }

  @default(ActivityNode) trait Fire {
    type OActivityNode = ListBuffer[Token] => Unit

    override def activityNode = _ => _ => {}

    override def action = node => _ => {
      node.doAction
      node.sendOffers
    }

    override def activityFinalNode = node => _ =>
      node.activity.terminateNodes

    override def decisionNode = node => tokens => {
      val selectedEdge = node.outgoing.find { edge =>
        edge match {
          case e: ControlFlow => e.guard.currentValue match {
            case v: BooleanValue =>  v.value
            case _ => false
          }
          case _ => false
        }
      }
      if (!selectedEdge.isEmpty) {
        node.addTokens(tokens)
        selectedEdge.get.sendOffer(tokens)
      }
    }

    override def initialNode = node => tokens => {
      val producedTokens = ListBuffer[Token]()
      producedTokens += new ControlToken
      node.addTokens(producedTokens)
      node.sendOffers(producedTokens)
    }

    override def forkNode = node => tokens => {
      val forkedTokens = ListBuffer[Token]()
      tokens.foreach { token =>
        val forkedToken = new ForkedToken
        forkedToken.baseToken = token
        forkedToken.remainingOffersCount = node.outgoing.size
        forkedTokens += forkedToken
      }
      node.addTokens(forkedTokens)
      node.sendOffers(forkedTokens)
    }

    override def controlNode = node => tokens => {
      node.addTokens(tokens)
      node.sendOffers(tokens)
    }
  }


  @adt trait ActivityNode {
    val name: String
    val outgoing = ListBuffer[ActivityEdge]()
    val incoming = ListBuffer[ActivityEdge]()
    var activity: Activity = null
    val heldTokens = ListBuffer[Token]()
    var running: Boolean = false
    def run { running = true }
    def isRunning = running
    def terminate { running = false }
    def isReady = isRunning

    def sendOffers(tokens: ListBuffer[Token]) =
      outgoing.foreach { _.sendOffer(tokens) }

    def takeOffedTokens: ListBuffer[Token] = {
      val allTokens = ListBuffer[Token]()
      incoming.foreach { edge =>
        val tokens = edge.takeOfferedTokens
        tokens.foreach(_.withdraw)
        allTokens ++= tokens
      }
      allTokens
    }
    def addTokens(tokens: ListBuffer[Token]) {
      tokens.foreach { token =>
        val transferredToken = token.transfer(this)
        heldTokens += transferredToken
      }
    }
    def hasOffers = {
      var hasOffer = true
      incoming.foreach { edge =>
        if (!edge.hasOffer) {
          hasOffer = false
        }
      }
      //println(name + " hasOffers: " + hasOffer)
      hasOffer
    }
    def removeToken(token: Token) =
      heldTokens -= token

    trait ExecutableNode

    // variants
    trait Action extends ExecutableNode {
      def doAction {}
      def sendOffers {
        if (outgoing.size > 0) {
          val tokens = ListBuffer[Token]()
          tokens += new ControlToken {}
          addTokens(tokens)
          outgoing(0).sendOffer(tokens)
        }
      }
    }
    class ActivityFinalNode(val name: String) extends FinalNode
    trait ControlNode extends ActivityNode
    class DecisionNode(val name: String) extends ControlNode
    trait FinalNode extends ControlNode
    class ForkNode(val name: String) extends ControlNode
    class InitialNode(val name: String) extends ControlNode
    class JoinNode(val name: String) extends ControlNode
    class MergeNode(val name: String) extends ControlNode {
      override def hasOffers: Boolean = {
        incoming.foreach { edge =>
          if (edge.hasOffer) {
            return true
          }
        }
        false
      }
    }
    class OpaqueAction(val name: String) extends Action {
      val expressions = ListBuffer[Expression]()
      override def doAction {
        expressions.foreach(execute(_))
      }
    }
  }

  trait Input {
    var inputValues = ListBuffer[InputValue]()
  }
  trait InputValue {
    var value: Value = null
    var variable: Variable = null
  }

  @adt trait Token {
    var holder: ActivityNode = null

    def transfer(holder: ActivityNode) = {
      if (this.holder != null) {
        withdraw
      }
      this.holder = holder
      this
    }

    def withdraw {
      if (!isWithdrawn) {
        holder.removeToken(this)
        holder = null
      }
    }

    def isWithdrawn = holder == null

    class ControlToken

    class ForkedToken {
      var baseToken: Token = null
      var remainingOffersCount = 0 // ???
    }
  }

  class Offer {
    val offeredTokens = ListBuffer[Token]()
    def hasTokens = {
      //println("before # of tokens: " + offeredTokens.size)
      removeWithdrawnTokens
      //println("after # of tokens: " + offeredTokens.size)
      offeredTokens.size > 0
    }
    def removeWithdrawnTokens {
      val tokensToBeRemoved = ListBuffer[Token]()
      offeredTokens.foreach { token =>
        if (token.isWithdrawn) {
          tokensToBeRemoved += token
        }
      }
      offeredTokens --= tokensToBeRemoved
    }
  }

  class Trace(val executedNodes: ListBuffer[ActivityNode] = ListBuffer())
}
