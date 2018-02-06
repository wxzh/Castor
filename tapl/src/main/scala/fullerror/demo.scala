package tapl.fullerror

import FullError._

object FullErrorDemo extends util.Demo[Context, Command] {
  import util.Print._

  val width = 60

  override val initialContext: Context = Context()
  override val defaultExample: String = "examples/fullerror.tapl"

  override def parseInput(s: String): List[Command] =
    FullErrorParsers.input(s)(Context())._1

//  private def checkBinding(ctx: Context, bind: Binding): Binding = bind match {
//    case NameBind =>
//      NameBind
//    case VarBind(tyT) =>
//      VarBind(tyT)
//    case TmAbbBind(t, None) =>
//      TmAbbBind(t, Some(typeof(ctx, t)))
//    case TmAbbBind(t, Some(tyT)) =>
//      val tyT1 = typeof(ctx, t)
//      if (subtype(ctx, tyT1, tyT))
//        TmAbbBind(t, Some(tyT))
//      else
//        throw new Exception("type of binding doesn't match declared type in " + bind)
//    case TyVarBind =>
//      TyVarBind
//    case TyAbbBind(tyT) =>
//      TyAbbBind(tyT)
//  }

  def processCommand(ctx: Context, cmd: Command): Context = cmd match {
    case Eval(t1) =>
      val ty1 = typeof(t1)(ctx)
      val doc1 = g2(ptmATerm(t1)(true, ctx) :: ":" :/: ptyType(ty1)(true,ctx) :: ";")

      val t2 = eval(ctx, t1)
      val ty2 = typeof(t2)(ctx)
      val doc2 = g2(ptmATerm(t2)(true, ctx) :: ":" :/: ptyType(ty2)(true,ctx) :: ";")

      println("====================")
      println(print(doc1, width))
      println("""||""")
      println("""\/""")
      println(print(doc2, width))

      ctx

    case Bind(x, bind) =>
//      val bind1 = checkBinding(ctx, bind)
//      val bind2 = evalBinding(ctx, bind1)
//      val bind2 = evalBinding(ctx, bind)
      val doc1 = x :: pBindingTy(bind)(ctx) :: ";"
      println("====================")
      println(print(doc1, width))

      ctx.addBinding(x, bind)
  }

}
