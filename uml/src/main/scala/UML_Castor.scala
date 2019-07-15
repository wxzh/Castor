package uml

import examples._
import collection.mutable._

@family trait ExpModel {
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
}

@family
@adts(IntegerCalculationOperator,IntegerComparisonOperator,BooleanUnaryOperator,BooleanBinaryOperator,Expression,Value,Variable)
trait ExpLang extends ExpModel {
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

@family
//@adts(IntegerCalculationOperator,IntegerComparisonOperator,BooleanUnaryOperator,BooleanBinaryOperator,Expression,Value,Variable)
trait UmlModel extends ExpModel {
  trait Activity {
    val name: String
    var nodes = ListBuffer[ActivityNode]()
    var edges = ListBuffer[Edge]()
    var locals = ListBuffer[Variable]()
    var inputs = ListBuffer[Variable]()
    var trace: Trace = null
  }
  @adt trait Edge {
    var offers = ListBuffer[Offer]()
    val name: String
    val source, target: ActivityNode
    class ActivityEdge(val name: String, val source: ActivityNode, val target: ActivityNode)
    class ControlFlow(name: String, source: ActivityNode, target: ActivityNode, val guard: BooleanVariable)
      extends ActivityEdge(name,source,target)
  }

  @adt trait ActivityNode {
    val name: String
    val outgoing = ListBuffer[Edge]()
    val incoming = ListBuffer[Edge]()
    var activity: Activity = null
    val heldTokens = ListBuffer[Token]()
    var running: Boolean = false

    trait ExecutableNode
    trait Action extends ExecutableNode
    class ActivityFinalNode(val name: String) extends FinalNode
    trait ControlNode extends ActivityNode
    class DecisionNode(val name: String) extends ControlNode
    trait FinalNode extends ControlNode
    class ForkNode(val name: String) extends ControlNode
    class InitialNode(val name: String) extends ControlNode
    class JoinNode(val name: String) extends ControlNode
    class MergeNode(val name: String) extends ControlNode
    class OpaqueAction(val name: String) extends Action {
      val expressions = ListBuffer[Expression]()
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

    class ControlToken

    class ForkedToken {
      var baseToken: Token = null
      var remainingOffersCount = 0 // ???
    }
  }

  class Offer {
    val offeredTokens = ListBuffer[Token]()
  }

  class Trace(val executedNodes: ListBuffer[ActivityNode] = ListBuffer())
}

@family
@adts(Expression,BooleanUnaryOperator,BooleanBinaryOperator,IntegerCalculationOperator,IntegerComparisonOperator,ActivityNode,Edge,Token)
@ops(Execute)
trait UmlLang extends UmlModel with ExpLang {
  // Methods for activity
  def main(activity: Activity, inputValues: ListBuffer[InputValue]) {
    initialize(activity,inputValues)
    initializeTrace(activity)
    runActivity(activity)
  }
  def runActivity(activity: Activity) {
    runNodes(activity)
    fireInitialNode(activity)
    var enabledNodes = getEnabledNodes(activity)
    while (enabledNodes.size > 0) {
      enabledNodes.foreach { nextNode =>
        fireNode(activity,nextNode)
        enabledNodes = getEnabledNodes(activity)
      }
    }
  }
  def terminateNodes(activity: Activity) {
    activity.nodes.foreach {
      terminate(_)
    }
  }
  def initializeTrace(activity: Activity) {
    activity.trace = new Trace
  }
  def initialize(activity: Activity, inputValues: ListBuffer[InputValue]) {
    activity.locals.foreach { v =>
      v.currentValue = v.initialValue
    }
    if (inputValues != null) {
      inputValues.foreach { v =>
        v.variable.currentValue = v.value
      }
    }
    activity.nodes.foreach { _.activity = activity }
  }
  def runNodes(activity: Activity) {
    activity.nodes.foreach { node =>
      node.running = true
    }
  }
  def fireInitialNode(activity: Activity) {
    fireNode(activity, getInitialNode(activity))
  }
  def fireNode(activity: Activity, node: ActivityNode) {
  //          println("fire node " + node.name)
    val tokens = takeOfferedTokens(node)
    fire(node)(tokens)
    activity.trace.executedNodes += node
  }
  def getInitialNode(activity: Activity): InitialNode = {
    activity.nodes.foreach { node =>
      if (node.isInstanceOf[InitialNode]) {
        return node.asInstanceOf[InitialNode]
      }
    }
    null
  }
  def getEnabledNodes(activity: Activity) = {
    val enabledNodes = ListBuffer[ActivityNode]()
    activity.nodes.foreach { node =>
      if (isReady(node))
        enabledNodes += node
    }
    enabledNodes
  }

  @default(ActivityNode, Edge) trait TakeOfferedTokens {
    type OActivityNode = ListBuffer[Token]
    type OEdge = OActivityNode

    def activityNode = node => {
      val allTokens = ListBuffer[Token]()
      node.incoming.foreach { edge =>
        val tokens = this(edge)
        tokens.foreach(withdraw(_))
        allTokens ++= tokens
      }
      allTokens
    }

    def edge = e => {
      val tokens = ListBuffer[Token]()
      e.offers.foreach { o =>
        tokens ++= o.offeredTokens
      }
      e.offers.clear
      tokens
    }
  }

  // Methods for ActivityNode
  def isRunning(node: ActivityNode) = node.running
  def terminate(node: ActivityNode) { node.running = false }
  def addTokens(node: ActivityNode, tokens: ListBuffer[Token]) {
    tokens.foreach { token =>
      val transferredToken = transfer(token, node)
      node.heldTokens += transferredToken
    }
  }
  def removeToken(node: ActivityNode, token: Token) =
    node.heldTokens -= token
  def sendOffers(node: ActivityNode, tokens: ListBuffer[Token]) =
    node.outgoing.foreach { sendOffer(_, tokens) }
  // Methods for token
  def transfer(token: Token, holder: ActivityNode) = {
    if (token.holder != null) {
      withdraw(token)
    }
    token.holder = holder
    token
  }

  def withdraw(token: Token) {
    if (!isWithdrawn(token)) {
      removeToken(token.holder, token)
      token.holder = null
    }
  }

  def isWithdrawn(token: Token) = token.holder == null


  // Methods for Offer
  def hasTokens(offer: Offer) = {
    removeWithdrawnTokens(offer)
    offer.offeredTokens.size > 0
  }
  def removeWithdrawnTokens(offer: Offer) {
    val tokensToBeRemoved = ListBuffer[Token]()
    offer.offeredTokens.foreach { token =>
      if (isWithdrawn(token)) {
        tokensToBeRemoved += token
      }
    }
    offer.offeredTokens --= tokensToBeRemoved
  }

  // Methods for Edge
  def sendOffer(edge: Edge, tokens: ListBuffer[Token]) {
    val offer = new Offer{}
    tokens.foreach { token =>
      offer.offeredTokens += token
    }
    edge.offers += offer
  }

  def takeOfferedTokens(edge: Edge): ListBuffer[Token] = {
    val tokens = ListBuffer[Token]()
    edge.offers.foreach { o =>
        tokens ++= o.offeredTokens
    }
    edge.offers.clear
    tokens
  }
  def hasOffer(edge: Edge): Boolean = {
    //println("# of offers " + name + ":" + offers.size)
    edge.offers.foreach { o =>
      if (hasTokens(o)) {
         //println(name + " hasOffer: true")
         return true
      }
    }
    //println(name + " hasOffer: false")
    false
  }

  @default(ActivityNode) trait IsReady {
    type OActivityNode = Boolean

    override def initialNode = _ => false

    override def activityNode = node =>
      isRunning(node) && hasOffers(node)

    override def joinNode = node => {
      var ready = true
      node.incoming.foreach { edge =>
        if (!hasOffer(edge)) {
          ready = false
        }
      }
      ready
    }

    override def decisionNode = node => {
      var ready = true
      node.incoming.foreach { edge =>
        if (!hasOffer(edge))
          ready = false
      }
      ready
    }
  }

  @default(ActivityNode) trait HasOffers {
    type OActivityNode = Boolean

    def activityNode= node => {
      var flag = true
      node.incoming.foreach { edge =>
        if (!hasOffer(edge)) {
          flag = false
        }
      }
      //println(name + " hasOffers: " + hasOffer)
      flag
    }

    override def mergeNode = node =>
      node.incoming.exists{ hasOffer(_) }
  }

  @default(ActivityNode) trait Fire {
    type OActivityNode = ListBuffer[Token] => Unit

    override def activityNode = _ => _ => {}

    override def action = node => _ => {
      if (node.outgoing.size > 0) {
        val tokens = ListBuffer[Token]()
        tokens += new ControlToken {}
        addTokens(node,tokens)
        sendOffer(node.outgoing(0),tokens)
      }
    }

    override def opaqueAction = node => tokens => {
      node.expressions.foreach(execute(_))
      action(node)(tokens)
    }

    override def activityFinalNode = node => _ =>
      terminateNodes(node.activity)

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
        addTokens(node, tokens)
        sendOffer(selectedEdge.get, tokens)
      }
    }

    override def initialNode = node => tokens => {
      val producedTokens = ListBuffer[Token]()
      producedTokens += new ControlToken
      addTokens(node, producedTokens)
      sendOffers(node, producedTokens)
    }

    override def forkNode = node => tokens => {
      val forkedTokens = ListBuffer[Token]()
      tokens.foreach { token =>
        val forkedToken = new ForkedToken
        forkedToken.baseToken = token
        forkedToken.remainingOffersCount = node.outgoing.size
        forkedTokens += forkedToken
      }
      addTokens(node, forkedTokens)
      sendOffers(node, forkedTokens)
    }

    override def controlNode = node => tokens => {
      addTokens(node, tokens)
      sendOffers(node, tokens)
    }
  }
}

