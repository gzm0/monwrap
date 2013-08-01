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

  def appDynImpl[T : c.WeakTypeTag, M[_]](c: Context)(n: c.Expr[String])(arg: c.Expr[Any]*) 
  (implicit monTT: c.WeakTypeTag[M[_]]) = {
  
    import c.universe._

    /**
     * create a Select-like syntax tree for a MW. Calls unwrap first
     */
    def SelectMW(mw: Tree, fun: TermName) = {
      val mon = Apply(Select(Ident("monwrap"), "unwrap"), mw :: Nil)
      Select(mon, fun)
    }
 
    @tailrec
    def procArgs(
      args_in:  List[c.Expr[Any]],
      args_out: List[Tree],
      aInd: Int)(wrap: Tree => Tree): (List[Tree], Tree => Tree) = args_in match {
      case a :: as => isMWType[M](c)(a.tree.tpe) match {
        case Some(tpe) =>
          val tn = newTermName("arg" + aInd)
          procArgs(as, Ident(tn) :: args_out, aInd + 1){ t =>
            Apply(SelectMW(a.tree, "flatMap"), lambda(c)(tn -> tpe)(wrap(t)) :: Nil)
          }
        case None =>
          procArgs(as, a.tree :: args_out, aInd)(wrap)
      }
      case Nil => (args_out.reverse, wrap)
    }

    val (al, wrap) = procArgs(arg.toList, Nil, 0)(x => x)

    val Literal(Constant(fn: String)) = n.tree
    val fnEnc = newTermName(fn).encodedName.toTermName                                       

    val newCall = c.Expr[Any](Apply(Select(Ident("v"), fnEnc), al))
    val x = reify { v: T => newCall.splice }

    val rmon = wrap(Apply(SelectMW(c.prefix.tree, "map"), x.tree :: Nil))

    c.Expr[Any](Apply(Select(Ident("monwrap"),"wrap"), rmon :: Nil))

  }

  /**
   * check if the given type is of type MW[T,M] for some T and return
   * Some(typeOf[T]) or None otherwise
   */
  def isMWType[M[_]](c: Context)(t: c.Type)(implicit tt: c.WeakTypeTag[M[_]]) = {
    import c.universe._

    for {
      // Check if t is a MW
      t <- Some(t) if t <:< buildMWType[M](c)
      // Extract type of value
      TypeRef(_,  _, List(res,_)) = t
    } yield res
  }

  /**
   * build typeOf[MW[_,M]] given WeakTypeTag[M[_]]
   * thanks to Eugene Burmanko
   * http://stackoverflow.com/a/17791973/1149944
   */
  def buildMWType[M[_]](c: Context)(implicit tt: c.WeakTypeTag[M[_]]) = {
    import c.universe._

    val TypeRef(_, sym, _) = tt.tpe

    val templ = typeOf[MW[_,Hack]]
    templ.substituteSymbols(List(typeOf[Hack[_]].typeSymbol), List(sym))
  }

  /**
   * construct a syntax tree for an anomymous function
   * in context c with given parameters and body
   */
  def lambda(c: Context)(params: (c.TermName, c.Type)*)(b: c.Tree) = {
    import c.universe._
    import Flag._

    val mods = Modifiers(PARAM)

    val defs =
      for ((tn,tpe) <- params)
        yield ValDef(mods, tn, TypeTree(tpe), EmptyTree)

    Function(defs.toList, b)
  }

}
