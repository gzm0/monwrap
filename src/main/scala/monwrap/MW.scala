package monwrap

import language.experimental.macros
import language.dynamics
import language.postfixOps
import language.higherKinds

import scala.reflect.macros.Context

class MW[T,M[_]](
  val __mon: M[T]
) extends AnyVal with Dynamic {
  def applyDynamic(n: String)(arg: Any*) = macro MW.appDynImpl[T,M]
  override def toString = s"MW(${__mon.toString})"
}

object MW {

  def appDynImpl[T : c.WeakTypeTag, M[_]](c: Context)(n: c.Expr[String])(arg: c.Expr[Any]*) 
  (implicit monTT: c.WeakTypeTag[M[_]]) = {

    val mh = new MacroHelper[c.type](c)
  
    import c.universe._
    import mh._

    val (al, wrap) = procArgs[M](arg.toList)

    val Literal(Constant(fn: String)) = n.tree
    val fnEnc = newTermName(fn).encodedName.toTermName                                       

    val newCall = c.Expr[Any](Apply(Select(Ident("v"), fnEnc), al))
    val callLambda = reify { v: T => newCall.splice } tree

    val rmon = wrap(Apply(SelectMW(c.prefix.tree, "map"), callLambda :: Nil))

    c.Expr[Any](Apply(Select(Ident("monwrap"),"wrap"), rmon :: Nil))

  }




}
