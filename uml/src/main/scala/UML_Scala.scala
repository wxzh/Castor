package uml

import collection.mutable._

object Metamodel {
  // Expressions
  trait IExpression {
    def execute: Unit
  }
  trait IIntegerExpression extends IExpression {
    val operand1: IntegerVariable
    val operand2: IntegerVariable
  }
  trait IIntegerCalculationExpression extends IIntegerExpression {
    val assignee: IntegerVariable
    val operator: IntegerCalculationOperator
    val operand1: IntegerVariable
    val operand2: IntegerVariable
  }
  trait IIntegerComparisonExpression extends IIntegerExpression {
    val assignee: BooleanVariable
    val operator: IntegerComparisonOperator
    val operand1: IntegerVariable
    val operand2: IntegerVariable
  }
  trait IBooleanExpression extends IExpression {
    val assignee: BooleanVariable
  }
  trait IBooleanUnaryExpression extends IBooleanExpression {
    val operator: BooleanUnaryOperator
    val operand: BooleanVariable
  }
  trait IBooleanBinaryExpression extends IBooleanExpression {
    val operator: BooleanBinaryOperator
    val operand1: BooleanVariable
    val operand2: BooleanVariable
  }

  // Operators
  sealed trait BooleanUnaryOperator
  case object NOT extends BooleanUnaryOperator

  sealed trait BooleanBinaryOperator
  case object AND extends BooleanBinaryOperator
  case object OR extends BooleanBinaryOperator

  sealed trait IntegerCalculationOperator
  case object ADD extends IntegerCalculationOperator
  case object SUBTRACT extends IntegerCalculationOperator

  sealed trait IntegerComparisonOperator
  case object SMALLER extends IntegerComparisonOperator
  case object SMALLER_EQUALS extends IntegerComparisonOperator
  case object EQUALS extends IntegerComparisonOperator
  case object GREATER_EQUALS extends IntegerComparisonOperator
  case object GREATER extends IntegerComparisonOperator

  // Values
  trait Value
  class IntegerValue(val value: Int) extends Value
  class BooleanValue(val value: Boolean) extends Value

  // Variables
  trait Variable {
    val name: String
    val initialValue: Value
    var currentValue: Value = null
  }
  class IntegerVariable(val name: String, val initialValue: IntegerValue) extends Variable
  class BooleanVariable(val name: String, val initialValue: BooleanValue) extends Variable

  // Nodes
  trait IActivityNode {
    val name: String
    val outgoing = ListBuffer[IActivityEdge]()
    val incoming = ListBuffer[IActivityEdge]()
    var activity: IActivity = null
    val heldTokens = ListBuffer[IToken]()
    var running: Boolean = false
    def fire(tokens: ListBuffer[IToken]): Unit
    def run: Unit
    def isRunning: Boolean
    def terminate: Unit
    def isReady: Boolean

    def sendOffers(tokens: ListBuffer[IToken]): Unit
    def takeOffedTokens: ListBuffer[IToken]
    def addTokens(tokens: ListBuffer[IToken]): Unit
    def hasOffers: Boolean
    def removeToken(token: IToken): Unit
  }
  trait IExecutableNode extends IActivityNode
  trait IAction extends IExecutableNode
  trait IActivityFinalNode extends IFinalNode
  trait IControlNode extends IActivityNode
  trait IDecisionNode extends IControlNode
  trait IFinalNode extends IControlNode
  trait IForkNode extends IControlNode
  trait IInitialNode extends IControlNode
  trait IJoinNode extends IControlNode
  trait IMergeNode extends IControlNode
  trait IOpaqueAction extends IAction {
    val expressions = ListBuffer[IExpression]()
  }

  // Edges
  trait IActivityEdge {
    val name: String
    val source: IActivityNode
    val target: IActivityNode
    var offers = ListBuffer[IOffer]()

    def sendOffer(tokens: ListBuffer[IToken]): Unit
    def takeOfferedTokens: ListBuffer[IToken]
    def hasOffer: Boolean
  }
  trait IControlFlow extends IActivityEdge {
    val guard: BooleanVariable
  }

  // Activity
  trait IActivity {
    val name: String
    var nodes = ListBuffer[IActivityNode]()
    var edges = ListBuffer[IActivityEdge]()
    var locals = ListBuffer[Variable]()
    var inputs = ListBuffer[Variable]()
    var trace: Trace = null

    def terminateNodes: Unit
  }

  // Token
  trait IToken {
    var holder: IActivityNode = null
    def transfer(holder: IActivityNode): IToken
    def withdraw: Unit
    def isWithdrawn : Boolean
  }
  trait IControlToken extends IToken
  trait IForkedToken extends IToken {
    var baseToken: IToken = null
    var remainingOffersCount = 0 // ???
  }

  trait IOffer {
    val offeredTokens = ListBuffer[IToken]()
    def hasTokens: Boolean
  }

  trait Trace {
    val executedNodes: ListBuffer[IActivityNode] = ListBuffer()
  }

  trait Input {
    var inputValues = ListBuffer[InputValue]()
  }
  trait InputValue {
    var value: Value = null
    var variable: Variable = null
  }
}

object Lang {
  import Metamodel._
  trait Activity extends IActivity {
    val name: String
    def initializeTrace {
      trace = new Trace {}
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
    def fireNode(node: IActivityNode) {
//      println("fire node " + node.name)
      val tokens = node.takeOffedTokens
      node.fire(tokens)
      trace.executedNodes += node
    }
    def getInitialNode: IInitialNode = {
      nodes.foreach { node =>
        if (node.isInstanceOf[IInitialNode]) {
          return node.asInstanceOf[IInitialNode]
        }
      }
      null
    }
    def getEnabledNodes = {
      val enabledNodes = ListBuffer[IActivityNode]()
      nodes.foreach { node =>
        if (node.isReady)
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
  class ActivityEdge(val name: String, val source: IActivityNode, val target: IActivityNode) extends IActivityEdge {
    def sendOffer(tokens: ListBuffer[IToken]) {
      val offer = new Offer
      tokens.foreach { token =>
        offer.offeredTokens += token
      }
      offers += offer
    }

    def takeOfferedTokens = {
      val tokens = ListBuffer[IToken]()
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
  class ControlFlow(name: String, source: IActivityNode, target: IActivityNode, val guard: BooleanVariable)
    extends ActivityEdge(name, source, target) with IControlFlow

  trait Expression extends IExpression

  trait IntegerExpression extends IIntegerExpression with Expression {
    def getCurrentIntValue(variable: Variable) = {
      var currentValue: IntegerValue = null
      val value = variable.currentValue
      if (value.isInstanceOf[IntegerValue]) {
        currentValue = value.asInstanceOf[IntegerValue]
      }
      currentValue
    }
  }

  class IntegerCalculationExpression(val assignee: IntegerVariable,
                                         val operator: IntegerCalculationOperator,
                                         val operand1: IntegerVariable,
                                         val operand2: IntegerVariable)
    extends IntegerExpression with IIntegerCalculationExpression  {
    def execute = {
      val operandValue1 = getCurrentIntValue(operand1).value
      val operandValue2 = getCurrentIntValue(operand2).value
      var result: Integer = null
      operator match {
        case ADD => result = operandValue1 + operandValue2
        case SUBTRACT => result = operandValue1 - operandValue2
      }
      var resultValue: IntegerValue = new IntegerValue(result)
      assignee.currentValue = resultValue
    }
  }
  class IntegerComparisonExpression(val assignee: BooleanVariable,
                                        val operator: IntegerComparisonOperator,
                                        val operand1: IntegerVariable,
                                        val operand2: IntegerVariable)
    extends IntegerExpression with IIntegerComparisonExpression {

	  def execute = {
		  val operandValue1 = getCurrentIntValue(operand1).value
		  val operandValue2 = getCurrentIntValue(operand2).value

		  val result =
		  operator match {
			  case EQUALS => operandValue1 == operandValue2
			  case GREATER => operandValue1 > operandValue2
			  case GREATER_EQUALS => operandValue1 >= operandValue2
			  case SMALLER => operandValue1 < operandValue2
			  case SMALLER_EQUALS => operandValue1 <= operandValue2
		  }
		 val resultValue = new BooleanValue(result)
		 assignee.currentValue = resultValue
	  }
  }
  trait BooleanExpression extends Expression with IBooleanExpression {
    def getCurrentBoolValue(variable: Variable): BooleanValue = {
      var currentValue: BooleanValue = null
      val value = variable.currentValue
      if (value.isInstanceOf[BooleanValue]) {
        currentValue = value.asInstanceOf[BooleanValue]
      }
      currentValue
    }
    def assignValue(value: Boolean) = {
      var resultValue = new BooleanValue(value)
      assignee.currentValue = resultValue
    }
  }

  class BooleanUnaryExpression(val assignee: BooleanVariable,
                                   val operator: BooleanUnaryOperator,
                                   val operand: BooleanVariable) extends BooleanExpression with IBooleanUnaryExpression {
    def execute = {
      val operandValue = getCurrentBoolValue(operand).value
      val result =
      operator match {
        case NOT => !operandValue
      }
      assignValue(result)
    }
  }

  class BooleanBinaryExpression(val assignee: BooleanVariable,
                                    val operator: BooleanBinaryOperator,
                                    val operand1: BooleanVariable,
                                    val operand2: BooleanVariable)
    extends BooleanExpression with IBooleanBinaryExpression {
    def execute = {
		  val operandValue1 = getCurrentBoolValue(operand1).value
		  val operandValue2 = getCurrentBoolValue(operand2).value

		  var result = operator match {
			  case AND => operandValue1 && operandValue2;
		    case OR => operandValue1 || operandValue2;
		  }
		  assignValue(result)
	  }
  }


  trait Action extends ExecutableNode with IAction {
    def doAction {}
    override def fire(tokens: ListBuffer[IToken]) {
      doAction
      sendOffers
    }
    def sendOffers {
      if (outgoing.size > 0) {
        val tokens = ListBuffer[IToken]()
        tokens += new ControlToken
        addTokens(tokens)
        outgoing(0).sendOffer(tokens)
      }
    }
    override def isReady = isRunning && hasOffers
  }
  class ActivityFinalNode(val name: String) extends FinalNode with IActivityFinalNode {
    override def fire(tokens: ListBuffer[IToken]) {
      activity.terminateNodes
    }
  }

  trait ActivityNode extends IActivityNode {
    val name: String
    def fire(tokens: ListBuffer[IToken]) {}
    def run { running = true }
    def isRunning = running
    def terminate { running = false }
    def isReady = isRunning

    def sendOffers(tokens: ListBuffer[IToken]) =
      outgoing.foreach { _.sendOffer(tokens) }

    def takeOffedTokens: ListBuffer[IToken] = {
      val allTokens = ListBuffer[IToken]()
      incoming.foreach { edge =>
        val tokens = edge.takeOfferedTokens
        tokens.foreach(_.withdraw)
        allTokens ++= tokens
      }
      allTokens
    }
    def addTokens(tokens: ListBuffer[IToken]) {
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
    def removeToken(token: IToken) =
      heldTokens -= token
  }
  trait ControlNode extends ActivityNode with IControlNode {
    override def fire(tokens: ListBuffer[IToken]) {
      addTokens(tokens)
      sendOffers(tokens)
    }
    override def isReady = isRunning && hasOffers
  }
  class DecisionNode(val name: String) extends ControlNode with IDecisionNode {
    override def fire(tokens: ListBuffer[IToken]) {
      val selectedEdge = outgoing.find { edge =>
        edge match {
          case e: ControlFlow => e.guard.currentValue match {
            case v: BooleanValue => v.value
            case _ => false
          }
          case _ => false
        }
      }
      if (!selectedEdge.isEmpty) {
        addTokens(tokens)
        selectedEdge.get.sendOffer(tokens)
      }
    }
    override def isReady = {
      var ready = true
      incoming.foreach { edge =>
        if (!edge.hasOffer)
          ready = false
      }
      ready
    }

  }
  trait ExecutableNode extends ActivityNode with IExecutableNode
  trait FinalNode extends ControlNode with IFinalNode
  class ForkNode(val name: String) extends ControlNode with IForkNode {
    override def fire(tokens: ListBuffer[IToken]) {
      val forkedTokens = ListBuffer[IToken]()
      tokens.foreach { token =>
        val forkedToken = new ForkedToken
        forkedToken.baseToken = token
        forkedToken.remainingOffersCount = outgoing.size
        forkedTokens += forkedToken
      }
      addTokens(forkedTokens)
      sendOffers(forkedTokens)
    }
  }
  class InitialNode(val name: String) extends ControlNode with IInitialNode {
    override def fire(tokens: ListBuffer[IToken]) {
      val producedTokens = ListBuffer[IToken]()
      producedTokens += new ControlToken
      addTokens(producedTokens)
      sendOffers(producedTokens)
    }
    override def isReady = false
  }
  class JoinNode(val name: String) extends ControlNode with IJoinNode {
    override def isReady = {
      var ready = true
      incoming.foreach { edge =>
        if (!edge.hasOffer) {
          ready = false
        }
      }
      ready
    }
  }
  class MergeNode(val name: String) extends ControlNode with IMergeNode {
    override def hasOffers: Boolean = {
//      var ready = false

      incoming.exists{ _.hasOffer }
//      incoming.foreach { edge =>
//        if (edge.hasOffer) {
//          return true
//        }
//      }
//      false
    }
  }
  class OpaqueAction(val name: String) extends Action with IOpaqueAction {
    override def doAction {
      expressions.foreach(_.execute)
    }
  }

  trait Token extends IToken {
    def transfer(holder: IActivityNode) = {
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
  }
  class ControlToken extends Token with IControlToken
  class ForkedToken extends Token with IForkedToken

  class Offer extends IOffer {
    def hasTokens = {
//      println("before # of tokens: " + offeredTokens.size)
      removeWithdrawnTokens
//      println("after # of tokens: " + offeredTokens.size)
      offeredTokens.size > 0
    }
    def removeWithdrawnTokens {
      val tokensToBeRemoved = ListBuffer[IToken]()
      offeredTokens.foreach { token =>
        if (token.isWithdrawn) {
          tokensToBeRemoved += token
        }
      }
      offeredTokens --= tokensToBeRemoved
    }
  }
}
