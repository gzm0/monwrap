package monwrap

import language.experimental.macros
import language.dynamics

import scala.reflect.macros.Context

class MW[T,M[V]](
  val __mon: M[T]
) extends AnyVal with Dynamic {
  def applyDynamic(n: String)(arg: Any*) = macro MW.appDynImpl[T,M]
}

object MW {

  def appDynImpl[T : c.WeakTypeTag, M[V]](c: Context { type PrefixType = MW[T,M] })
  (n: c.Expr[String])(arg: c.Expr[Any]*) = {
  
    import c.universe._

    def call(on: Tree)(f: String)(args: c.Expr[Any]*) = {
      val fT = newTermName(f)
      val arglist = args.map(_.tree).toList
      c.Expr[Any](Apply(Select(on, fT), arglist))
    }

    val Literal(Constant(fn: String)) = n.tree

    val x = reify { v: T => call(Ident(newTermName("v")))(fn)(arg :_*).splice }

    val mon = reify { unwrap(c.prefix.splice) }

    call(mon.tree)("map")(x)

  }

}
