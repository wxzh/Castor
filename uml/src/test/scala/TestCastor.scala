import uml._

import org.scalatest._
import uml.UmlLang._
import collection.mutable._

class TestCastor extends FunSuite {
  def executeActivity(a: Activity): Trace =
    executeActivity(a, a.inputs.map(v => new InputValue {
      value = v.initialValue;
      variable = v
    }))

  def executeActivity(a: Activity, inputs: ListBuffer[InputValue]): Trace = {
    main(a,inputs)
    a.trace
  }

  def totalExecutionOrder(trace: Trace) = trace.executedNodes.toList.map(_.name)

  def partialExecutionOrder(trace: Trace, nodes: ActivityNode*) = {
    val l = nodes.map(trace.executedNodes.indexOf(_))
    l == l.sorted
  }

  def nodeNotExecuted(trace: Trace, node: ActivityNode) =
    !trace.executedNodes.contains(node)


  def currentValue(variable: IntegerVariable) = variable.currentValue match {
    case v: IntegerValue => v.value
  }

  def currentValue(variable: BooleanVariable) = variable.currentValue match {
    case v: BooleanValue => v.value
  }

  test("case1") {
    val trace = executeActivity(case1)
    assert(totalExecutionOrder(trace) === List("initialNode1", "action1", "finalNode1"))
  }
  test("case2") {
    val trace = executeActivity(case2)
    assert(partialExecutionOrder(trace, case2.initialNode2,
      case2.forkNode1, case2.action2, case2.joinNode1, case2.finalNode2))
    assert(partialExecutionOrder(trace, case2.initialNode2,
      case2.forkNode1, case2.action3, case2.joinNode1, case2.finalNode2))
  }

  test("case3") {
    val trace = executeActivity(case3)
    assert(totalExecutionOrder(trace) === List("initialNode3", "decisionNode1", "action4", "mergeNode1", "finalNode3"))
    assert(nodeNotExecuted(trace, case3.action5))
  }

  test("case4") {
    val trace = executeActivity(case4)
    assert(totalExecutionOrder(trace) === List("initialNode4", "action6", "action7", "action8", "action9", "finalNode4"))
    assert(3 === currentValue(case4.var3))
    assert(1 === currentValue(case4.var4))
    assert(2 === currentValue(case4.var5))
    assert(true === currentValue(case4.var6))
    assert(false === currentValue(case4.var7))
    assert(true === currentValue(case4.var8))
  }
  test("case5") {
    val trace = executeActivity(case5)
    assert(totalExecutionOrder(trace) === List("initialNode5", "action10", "finalNode5"))
    assert(10 === currentValue(case5.var9))
    assert(5 === currentValue(case5.var10))
    assert(15 === currentValue(case5.var11))
  }
  test("case6 false") {
    val case6f = case6(false)
    val trace = executeActivity(case6f)
    assert(totalExecutionOrder(trace) === List("initialNode6", "register", "decisionInternal",
      "assignToProjectExternal", "mergeAuthorizePayment", "authorizePayment", "finalNode6"))
    assert(nodeNotExecuted(trace, case6f.getWelcomePackage))
    assert(nodeNotExecuted(trace, case6f.forkGetWelcomePackage))
    assert(nodeNotExecuted(trace, case6f.assignToProject))
    assert(nodeNotExecuted(trace, case6f.addToWebsite))
    assert(nodeNotExecuted(trace, case6f.joinManagerInterview))
    assert(nodeNotExecuted(trace, case6f.managerInterview))
    assert(nodeNotExecuted(trace, case6f.managerReport))
  }
  test("case6 true") {
    val case6t = case6(true)
    val trace = executeActivity(case6t)
    assert(partialExecutionOrder(trace, case6t.initialNode6, case6t.register, case6t.decisionInternal,
      case6t.getWelcomePackage, case6t.forkGetWelcomePackage, case6t.joinManagerInterview, case6t.managerInterview, case6t.managerReport,
      case6t.mergeAuthorizePayment, case6t.authorizePayment, case6t.finalNode6))
    assert(partialExecutionOrder(trace, case6t.forkGetWelcomePackage, case6t.assignToProject, case6t.joinManagerInterview))
    assert(partialExecutionOrder(trace, case6t.forkGetWelcomePackage, case6t.addToWebsite, case6t.joinManagerInterview))

    assert(nodeNotExecuted(trace, case6t.assignToProjectExternal))
  }

  val case1 = new Activity {
    val action = new OpaqueAction("action1")
    val finalNode = new ActivityFinalNode("finalNode1")
    val initialNode = new InitialNode("initialNode1")
    val edge1 = new ActivityEdge("edge1", initialNode, action)
    val edge2 = new ActivityEdge("edge2", action, finalNode)

    initialNode.outgoing += edge1
    finalNode.incoming += edge2

    action.incoming += edge1
    action.outgoing += edge2

    val name = "test1"
    nodes += (action,initialNode,finalNode)
    edges += (edge1,edge2)
  }

  val case2 = new Activity {
    val initialNode2 = new InitialNode("initialNode2")
    val finalNode2 = new ActivityFinalNode("finalNode2")
    val forkNode1 = new ForkNode("forkNode1")
    val joinNode1 = new JoinNode("joinNode1")
    val action2 = new OpaqueAction("action2")
    val action3 = new OpaqueAction("action3")
    val edge3 = new ActivityEdge ("edge3",initialNode2,forkNode1)
    val edge4 = new ActivityEdge ("edge4",forkNode1,action2)
    val edge5 = new ActivityEdge ("edge5",forkNode1,action3)
    val edge6 = new ActivityEdge ("edge6",action2,joinNode1)
    val edge7 = new ActivityEdge ("edge7",action3,joinNode1)
    val edge8 = new ActivityEdge ("edge8",joinNode1,finalNode2)

    initialNode2.outgoing += edge3
    forkNode1.incoming += edge3
    forkNode1.outgoing += (edge4, edge5)
    action2.incoming += edge4
    action2.outgoing += edge6
    action3.incoming += edge5
    action3.outgoing += edge7
    joinNode1.incoming += (edge6,edge7)
    joinNode1.outgoing += edge8
    finalNode2.incoming += edge8

    val name = "test2"
    //    nodes += (initialNode2,forkNode1,action2,action3,joinNode1,finalNode2)
    //    edges += (edge3,edge4,edge5,edge6,edge7,edge8)
    nodes += (action3,action2,forkNode1,joinNode1,initialNode2,finalNode2)
    edges += (edge5,edge4,edge8,edge3,edge7,edge6)
  }

  val case3 = new Activity {
    val var1 = new BooleanVariable ("var1",new BooleanValue(true))
    val var2 = new BooleanVariable ("var2",new BooleanValue(false))
    val initialNode3 = new InitialNode("initialNode3")
    val decisionNode1 = new DecisionNode("decisionNode1")
    val action4 = new OpaqueAction("action4")
    val action5 = new OpaqueAction("action5")
    val mergeNode1 = new MergeNode("mergeNode1")
    val finalNode3 = new ActivityFinalNode("finalNode3")

    val edge9 = new ActivityEdge("edge9", initialNode3, decisionNode1)
    val edge10 = new ControlFlow ("edge10",decisionNode1, action4, var1)
    val edge11 = new ControlFlow ("edge11",decisionNode1,action5,var2)
    val edge12 = new ActivityEdge ("edge12",action4, mergeNode1)
    val edge13 = new ActivityEdge ("edge13",action5, mergeNode1)
    val edge14 = new ActivityEdge ("edge14",mergeNode1, finalNode3)

    initialNode3.outgoing += edge9
    decisionNode1.incoming += edge9
    decisionNode1.outgoing += (edge10,edge11)
    action4.incoming += edge10
    action4.outgoing += edge12
    action5.incoming += edge11
    action5.outgoing += edge13
    mergeNode1.incoming += (edge12,edge13)
    mergeNode1.outgoing += edge14
    finalNode3.incoming += edge14

    val name = "test3"
    locals += (var1,var2)
    nodes += (action5,action4,decisionNode1,mergeNode1,initialNode3,finalNode3)
    edges += (edge9,edge10,edge11,edge12,edge13,edge14)
  }

  // TRACE: initialNode4 action6 action7 action8 action9 finalNode4
  // var3 = 3, var4 = 1, var5 = 2, var6 = true, var7 = false, var8 = true
  val case4 = new Activity {
    val var3 = new IntegerVariable ("var3",new IntegerValue(0))
    val var4 = new IntegerVariable ("var4",new IntegerValue(1))
    val var5 = new IntegerVariable ("var5",new IntegerValue(2))
    val add1 = new IntegerCalculationExpression(var3,ADD,var3,var4)
    val add2 = new IntegerCalculationExpression(var3,ADD,var3,var5)
    val var6 = new BooleanVariable("var6", new BooleanValue(false))
    val var7 = new BooleanVariable("var7", new BooleanValue(true))
    val var8 = new BooleanVariable("var8", new BooleanValue(false))
    val smaller = new IntegerComparisonExpression(var6,SMALLER,var4,var5)
    val not = new BooleanUnaryExpression (var7,NOT,var6)
    val or = new BooleanBinaryExpression(var8,OR,var6,var7)
    val initialNode4 = new InitialNode("initialNode4")
    val action6 = new OpaqueAction("action6")
    action6.expressions += (add1,add2)
    val action7 = new OpaqueAction("action7")
    action7.expressions += smaller
    val action8 = new OpaqueAction("action8")
    action8.expressions += not
    val action9 = new OpaqueAction("action9")
    action9.expressions += or
    val finalNode4 = new ActivityFinalNode("finalNode4")
    val edge15 = new ActivityEdge ("edge15",initialNode4, action6)
    val edge16 = new ActivityEdge ("edge16",action6, action7)
    val edge17 = new ActivityEdge ("edge17",action7, action8)
    val edge18 = new ActivityEdge ("edge18",action8, action9)
    val edge19 = new ActivityEdge ("edge19",action9, finalNode4)
    initialNode4.outgoing += edge15
    action6.incoming += edge15
    action6.outgoing += edge16
    action7.incoming += edge16
    action7.outgoing += edge17
    action8.incoming += edge17
    action8.outgoing += edge18
    action9.incoming += edge18
    action9.outgoing += edge19
    finalNode4.incoming += edge19

    val name = "test4"
    locals += (var3,var4,var5,var6,var7,var8)
    nodes += (initialNode4,action6,action7,action8,action9,finalNode4)
    edges += (edge15,edge16,edge17,edge18,edge19)
  }

  // TRACE: initialNode5 action10 finalNode5
  // var9 = 10, var10 = 5, var11 = 15
  val case5 = new Activity {
    val var9 = new IntegerVariable ("var9",new IntegerValue(10))
    val var10 = new IntegerVariable ("var10",new IntegerValue(5))
    val var11 = new IntegerVariable ("var11",new IntegerValue(0))
    val add = new IntegerCalculationExpression(var11,ADD,var9,var10)
    val initialNode5 = new InitialNode("initialNode5")
    val action10 = new OpaqueAction("action10")
    action10.expressions += add
    val finalNode5 = new ActivityFinalNode("finalNode5")
    val edge20 = new ActivityEdge ("edge20",initialNode5, action10)
    val edge21 = new ActivityEdge ("edge21",action10, finalNode5)

    val name = "test5"
    locals += (var10,var11)
    inputs += var9
    nodes += (initialNode5,action10,finalNode5)
    edges += (edge20,edge21)
  }


  def case6(b: Boolean) = new Activity {
    val notinternal = new BooleanVariable ("notinternal",new BooleanValue(false))
    val internal = new BooleanVariable ("internal",new BooleanValue(b))
    val initialNode6 = new InitialNode("initialNode6")
    val mergeAuthorizePayment = new MergeNode("mergeAuthorizePayment")
    val assignToProjectExternal = new OpaqueAction("assignToProjectExternal")
    val decisionInternal = new DecisionNode("decisionInternal")
    val finalNode6 = new ActivityFinalNode("finalNode6")
    val addToWebsite = new OpaqueAction("addToWebsite")
    val forkGetWelcomePackage = new ForkNode("forkGetWelcomePackage")
    val authorizePayment = new OpaqueAction("authorizePayment")
    val managerReport = new OpaqueAction("managerReport")
    val register = new OpaqueAction("register")
    register.expressions += new BooleanUnaryExpression(notinternal,NOT,internal)
    val assignToProject = new OpaqueAction("assignToProject")
    val joinManagerInterview = new JoinNode("joinManagerInterview")
    val getWelcomePackage = new OpaqueAction("getWelcomePackage")
    val managerInterview = new OpaqueAction("managerInterview")
    val edge42 = new ActivityEdge ("edge42",initialNode6, register)
    val edge43 = new ActivityEdge ("edge43",register, decisionInternal)
    val edge44 = new ControlFlow ("edge44",decisionInternal, assignToProjectExternal,notinternal)
    val edge45 = new ControlFlow ("edge45",decisionInternal, getWelcomePackage, internal)
    val edge46 = new ActivityEdge ("edge46",getWelcomePackage, forkGetWelcomePackage)
    val edge47 = new ActivityEdge ("edge47",forkGetWelcomePackage, assignToProject)
    val edge48 = new ActivityEdge ("edge48",forkGetWelcomePackage, addToWebsite)
    val edge49 = new ActivityEdge ("edge49",assignToProject, joinManagerInterview)
    val edge50 = new ActivityEdge ("edge50",addToWebsite, joinManagerInterview)
    val edge51 = new ActivityEdge ("edge51",joinManagerInterview, managerInterview)
    val edge52 = new ActivityEdge ("edge52",managerInterview, managerReport)
    val edge53 = new ActivityEdge ("edge53",managerReport, mergeAuthorizePayment)
    val edge54 = new ActivityEdge ("edge54",mergeAuthorizePayment, authorizePayment)
    val edge55 = new ActivityEdge ("edge55",authorizePayment, finalNode6)
    val edge56 = new ActivityEdge ("edge56",assignToProjectExternal, mergeAuthorizePayment)

    initialNode6.outgoing += edge42
    register.incoming += edge42
    register.outgoing += edge43
    decisionInternal.incoming += edge43
    decisionInternal.outgoing += (edge44,edge45)
    assignToProjectExternal.incoming += edge44
    assignToProjectExternal.outgoing += edge56
    getWelcomePackage.incoming += edge45
    getWelcomePackage.outgoing += edge46
    forkGetWelcomePackage.incoming += edge46
    forkGetWelcomePackage.outgoing += (edge47,edge48)
    assignToProject.incoming += edge47
    assignToProject.outgoing += edge49
    addToWebsite.incoming += edge48
    addToWebsite.outgoing += edge50
    joinManagerInterview.incoming += (edge49,edge50)
    joinManagerInterview.outgoing += edge51
    managerInterview.incoming += edge51
    managerInterview.outgoing += edge52
    managerReport.incoming += edge52
    managerReport.outgoing += edge53
    mergeAuthorizePayment.incoming += (edge53,edge56)
    mergeAuthorizePayment.outgoing += edge54
    authorizePayment.incoming += edge54
    authorizePayment.outgoing += edge55
    finalNode6.incoming += edge55

    val name = "test6"
    locals += notinternal
    inputs += internal
    nodes += (initialNode6,register,decisionInternal,assignToProjectExternal,getWelcomePackage,forkGetWelcomePackage,
      assignToProject,addToWebsite,joinManagerInterview,managerInterview,managerReport,mergeAuthorizePayment,authorizePayment,finalNode6)
    edges += (edge42,edge43,edge44,edge45,edge46,edge47,edge48,edge49,edge50,edge51,edge52,edge53,edge54,edge55,edge56)
  }
  def printActivity(a: Activity) = {
    println("Activity: " + a.name)
    a.nodes.foreach(activityNode => {
      println("> Node: " + activityNode.name)

      println("\t from: ")
      activityNode.incoming.foreach(edge => println("\t > incomingEdge: " + edge.name))

      println("\t to: ")
      activityNode.outgoing.foreach(edge => println("\t > outgoingEdge: " + edge.name))
    })
  }
}

