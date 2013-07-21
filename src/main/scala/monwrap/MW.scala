package monwrap

import language.experimental.macros
import language.dynamics
import language.postfixOps
import language.higherKinds

import scala.annotation.tailrec
import scala.reflect.macros.Context

class MW[T,M[_]](
  val __mon: M[T]
) extends AnyVal with Dynamic {
  def applyDynamic(n: String)(arg: Any*) = macro MW.appDynImpl[T,M]
}

object MW {

  def appDynImpl[T : c.WeakTypeTag, M[_]](c: Context { type PrefixType = MW[T,M] })
                                         (n: c.Expr[String])
                                         (arg: c.Expr[Any]*) 
                                         (implicit monTT: c.WeakTypeTag[M[_]]) = {
  
    import c.universe._

    def callT(on: Tree)(f: String)(args: Tree*): Tree = {
      val fT = newTermName(f)
      Apply(Select(on, fT), args.toList)
    }                                       
    def callE(on: Tree)(f: String)(args: c.Expr[Any]*): c.Expr[Any] =
      c.Expr[Any](callT(on)(f)(args.map(_.tree) :_*))

    def anfun(tns: TermName*)(tpes: TypeTree*)(b: Tree) = {
      import Flag._
      val mods = Modifiers(PARAM)
      val defs = for ((tn,tpe) <- tns zip tpes) yield ValDef(mods, tn, tpe, EmptyTree)
      Function(defs.toList, b)
    }

    def mwType(t: Type) = {
      val sym = weakTypeOf[MW[_,M]].typeSymbol
      val mSym = weakTypeOf[M[_]].typeSymbol
      t match {
        // TODO check for correct package!
        case TypeRef(_, `sym`, List(t,TypeRef(_,`mSym`,_))) => Some(t)
        case _ => None
      }
    }
 
    @tailrec
    def procArgs(
      args_in:  List[c.Expr[Any]],
      args_out: List[Tree],
      aInd: Int)(wrap: Tree => Tree): (List[Tree], Tree => Tree) = args_in match {
      case a :: as => mwType(a.tree.tpe) match {
        case Some(tpe) =>
          val tn = newTermName("arg" + aInd)
          val mon = callT(Ident(newTermName("monwrap")))("unwrap")(a.tree)
          procArgs(as, Ident(tn) :: args_out, aInd + 1){ t =>
            callT(mon)("flatMap")(anfun(tn)(TypeTree(tpe))(wrap(t)))
          }
        case None =>
          procArgs(as, a.tree :: args_out, aInd)(wrap)
      }
      case Nil => (args_out.reverse, wrap)
    }

    val (al, wrap) = procArgs(arg.toList, Nil, 0)(x => x)

    val Literal(Constant(fn: String)) = n.tree

    val x = reify { v: T => c.Expr[Any](callT(Ident(newTermName("v")))(fn)(al :_*)).splice }

    val mon = reify { unwrap(c.prefix.splice) }

    c.Expr[Any](wrap(callE(mon.tree)("map")(x).tree))

  }

}
