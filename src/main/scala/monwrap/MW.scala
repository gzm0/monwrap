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
  override def toString = s"MW(${__mon.toString})"
}

object MW {

  def appDynImpl[T : c.WeakTypeTag, M[_]](c: Context { type PrefixType = MW[T,M] })
                                         (n: c.Expr[String])
                                         (arg: c.Expr[Any]*) 
                                         (implicit monTT: c.WeakTypeTag[M[_]]) = {
  
    import c.universe._

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
      args_out: List[Tree])(wrap: Tree => Tree): (List[Tree], Tree => Tree) = args_in match {
      case a :: as => mwType(a.tree.tpe) match {
        case Some(tpe) =>
          val tn = newTermName(c.fresh())
          val mon = reify { unwrap(c.Expr[MW[_,M]](a.tree).splice) }
          procArgs(as, Ident(tn) :: args_out){ t =>
            Apply(Select(mon.tree, "flatMap"), 
                  anfun(tn)(TypeTree(tpe))(wrap(t)) :: Nil)
          }
        case None =>
          procArgs(as, a.tree :: args_out)(wrap)
      }
      case Nil => (args_out.reverse, wrap)
    }

    val (al, wrap) = procArgs(arg.toList, Nil)(x => x)

    val Literal(Constant(fn: String)) = n.tree
    val fnEnc = newTermName(fn).encodedName.toTermName                                       

    val vn = newTermName(c.fresh())
    val innerCall = anfun(vn)(TypeTree(weakTypeOf[T]))(Apply(Select(Ident(vn), fnEnc), al))

    val mon = reify { unwrap(c.prefix.splice) }

    val rmon = wrap(Apply(Select(mon.tree, "map"), innerCall :: Nil))

    c.Expr[Any](Apply(Select(Ident("monwrap"),"wrap"), rmon :: Nil))

  }

}
