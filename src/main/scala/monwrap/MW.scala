package monwrap

import language.experimental.macros
import language.dynamics

import scala.reflect.macros.Context

class MW[T](
    private[monwrap] val __mon: Option[T]
) extends Dynamic {

  def applyDynamic(n: String)(arg: Any*) = macro MW.appDynImpl[T]
}

object MW {

  def appDynImpl[T](c: Context { type PrefixType = MW[T] })
    (n: c.Expr[String])(arg: c.Expr[Any]*) = {
    import c.universe._

    val Literal(Constant(fn: String)) = n.tree

    def call(mon: Tree) = {
      val arglist = arg.map(_.tree).toList
      val fnT = newTermName(fn)
      c.Expr[Any](Apply(Select(mon, fnT), arglist))
    }

    reify {
      unwrap(c.prefix.splice).map(v => call(Ident(newTermName("v"))).splice)
    }

  }

}
