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
    def procArgs(ain:  List[c.Expr[Any]],
                 aout: List[Tree],
                 aInd: Int,
                 wrap: Tree => Tree): (List[Tree], Tree => Tree) = ain match {
      case a :: as => 
        val (out, naInd, nwrap) = procArg(a.tree, aInd, wrap)
        procArgs(as, out :: aout, naInd, nwrap)
      case Nil => (aout.reverse, wrap)
    }

    def procArg(in: Tree, aInd: Int, wrap: Tree => Tree) = ckMWType[M](c)(in.tpe) match {
      case Some(vTpe) =>
        val tn = newTermName("arg" + aInd)
        val nwrap = { t: Tree =>
          Apply(SelectMW(in, "flatMap"), lambda(c)(tn -> vTpe)(wrap(t)) :: Nil) }

        (Ident(tn), aInd + 1, nwrap)
      case None =>
        (in, aInd, wrap)
    }

    val (al, wrap) = procArgs(arg.toList, Nil, 0, x => x)

    val Literal(Constant(fn: String)) = n.tree
    val fnEnc = newTermName(fn).encodedName.toTermName                                       

    val newCall = c.Expr[Any](Apply(Select(Ident("v"), fnEnc), al))
    val callLambda = reify { v: T => newCall.splice } tree

    val rmon = wrap(Apply(SelectMW(c.prefix.tree, "map"), callLambda :: Nil))

    c.Expr[Any](Apply(Select(Ident("monwrap"),"wrap"), rmon :: Nil))

  }

  /**
   * check if the given type is of type MW[T,M] for some T and return
   * Some(typeOf[T]) or None otherwise
   */
  def ckMWType[M[_]](c: Context)(t: c.Type)(implicit tt: c.WeakTypeTag[M[_]]) = {
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
