package uml

import collection.mutable._

object Original {
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
      node.fire(tokens)
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
    extends ActivityEdge(name, source, target)

  trait Expression {
    def execute: Unit
  }
  trait IntegerExpression extends Expression {
    val operand1: IntegerVariable
    val operand2: IntegerVariable
    def getCurrentIntValue(variable: Variable) = {
      var currentValue: IntegerValue = null
      val value = variable.currentValue
      if (value.isInstanceOf[IntegerValue]) {
        currentValue = value.asInstanceOf[IntegerValue]
      }
      currentValue
    }
  }
  class IntegerCalculationExpression
    (val assignee: IntegerVariable,
     val operator: IntegerCalculationOperator,
     val operand1: IntegerVariable,
     val operand2: IntegerVariable) extends IntegerExpression {
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
  class IntegerComparisonExpression
    (val assignee: BooleanVariable,
     val operator: IntegerComparisonOperator,
     val operand1: IntegerVariable,
     val operand2: IntegerVariable) extends IntegerExpression {

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
  sealed trait IntegerCalculationOperator
  case object ADD extends IntegerCalculationOperator
  case object SUBTRACT extends IntegerCalculationOperator

  sealed trait IntegerComparisonOperator
  case object SMALLER extends IntegerComparisonOperator
  case object SMALLER_EQUALS extends IntegerComparisonOperator
  case object EQUALS extends IntegerComparisonOperator
  case object GREATER_EQUALS extends IntegerComparisonOperator
  case object GREATER extends IntegerComparisonOperator

  trait BooleanExpression extends Expression {
    val assignee: BooleanVariable
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

  class BooleanUnaryExpression(val assignee: BooleanVariable, operator: BooleanUnaryOperator, operand: BooleanVariable) extends BooleanExpression {
    def execute = {
      val operandValue = getCurrentBoolValue(operand).value
      val result =
      operator match {
        case NOT => !operandValue
      }
      assignValue(result)
    }
  }

  class BooleanBinaryExpression
    (val assignee: BooleanVariable,
     operator: BooleanBinaryOperator,
     operand1: BooleanVariable,
     operand2: BooleanVariable) extends BooleanExpression {
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

  sealed trait BooleanUnaryOperator
  case object NOT extends BooleanUnaryOperator

  sealed trait BooleanBinaryOperator
  case object AND extends BooleanBinaryOperator
  case object OR extends BooleanBinaryOperator

  trait Action extends ExecutableNode {
    def doAction {}
    override def fire(tokens: ListBuffer[Token]) {
      doAction
      sendOffers
    }
    def sendOffers {
      if (outgoing.size > 0) {
        val tokens = ListBuffer[Token]()
        tokens += new ControlToken {}
        addTokens(tokens)
        outgoing(0).sendOffer(tokens)
      }
    }
    override def isReady = isRunning && hasOffers
  }
  class ActivityFinalNode(val name: String) extends FinalNode {
    override def fire(tokens: ListBuffer[Token]) {
      activity.terminateNodes
    }
  }

  trait ActivityNode {
    val name: String
    val outgoing = ListBuffer[ActivityEdge]()
    val incoming = ListBuffer[ActivityEdge]()
    var activity: Activity = null
    val heldTokens = ListBuffer[Token]()
    var running: Boolean = false
    def fire(tokens: ListBuffer[Token]) {}
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
  }
  trait ControlNode extends ActivityNode {
    override def fire(tokens: ListBuffer[Token]) {
      addTokens(tokens)
      sendOffers(tokens)
    }
    override def isReady = isRunning && hasOffers
  }
  class DecisionNode(val name: String) extends ControlNode {
    override def fire(tokens: ListBuffer[Token]) {
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
//      val selectedEdge: ActivityEdge = null
//      outgoing.foreach { edge =>
//        if (edge isInstanceOf [ControlFlow]) {
//          val controlFlow = edge asInstanceOf [ControlFlow]
//          val guardValue = controlFlow.guard.currentValue
//
//          if (guardValue isInstanceOf [BooleanValue]) {
//            val booleanValue = guardValue asInstanceOf [BooleanValue]
//            if (booleanValue.value) {
//              selectedEdge = edge
//            }
//          }
//        }
//      }
    override def isReady = {
      var ready = true
      incoming.foreach { edge =>
        if (!edge.hasOffer)
          ready = false
      }
      ready
    }

  }
  trait ExecutableNode extends ActivityNode
  trait FinalNode extends ControlNode
  class ForkNode(val name: String) extends ControlNode {
    override def fire(tokens: ListBuffer[Token]) {
      val forkedTokens = ListBuffer[Token]()
      tokens.foreach { token =>
        val forkedToken = new ForkedToken {}
        forkedToken.baseToken = token
        forkedToken.remainingOffersCount = outgoing.size
        forkedTokens += forkedToken
      }
      addTokens(forkedTokens)
      sendOffers(forkedTokens)
    }
  }
  class InitialNode(val name: String) extends ControlNode {
    override def fire(tokens: ListBuffer[Token]) {
      val producedTokens = ListBuffer[Token]()
      producedTokens += new ControlToken{}
      addTokens(producedTokens)
      sendOffers(producedTokens)
    }
    override def isReady = false
  }
  class JoinNode(val name: String) extends ControlNode {
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
  class MergeNode(val name: String) extends ControlNode {
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
  class OpaqueAction(val name: String) extends Action {
    val expressions = ListBuffer[Expression]()
    override def doAction {
      expressions.foreach(_.execute)
    }
  }

  trait Value
  class IntegerValue(val value: Int) extends Value
  class BooleanValue(val value: Boolean) extends Value

  trait Variable {
    val name: String
    val initialValue: Value
    var currentValue: Value = null
  }
  class IntegerVariable(val name: String, val initialValue: Value) extends Variable
  class BooleanVariable(val name: String, val initialValue: Value) extends Variable

  trait Input {
    var inputValues = ListBuffer[InputValue]()
  }
  trait InputValue {
    var value: Value = null
    var variable: Variable = null
  }

  trait Token {
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
  }
  trait ControlToken extends Token
  trait ForkedToken extends Token {
    var baseToken: Token = null
    var remainingOffersCount = 0 // ???
  }

  class Offer {
    val offeredTokens = ListBuffer[Token]()
    def hasTokens = {
//      println("before # of tokens: " + offeredTokens.size)
      removeWithdrawnTokens
//      println("after # of tokens: " + offeredTokens.size)
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

  class Trace(
    val executedNodes: ListBuffer[ActivityNode] = ListBuffer()
  )
}
