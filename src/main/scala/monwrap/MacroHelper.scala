package monwrap

import scala.reflect.macros.Context
import scala.annotation.tailrec

private[monwrap] class MacroHelper[T <: Context](val mh_c: T) {

  import mh_c.universe._
  
  /**
   * create a Select-like syntax tree for a MW. Calls unwrap first
   */
  def SelectMW(mw: Tree, fun: TermName) = {
    val mon = Apply(Select(Ident("monwrap"), "unwrap"), mw :: Nil)
    Select(mon, fun)
  }

  def SelectFW(fw: Tree, fun: TermName) = {
    val fun = Apply(Select(Ident("monwrap"), "funwrap"), fw :: Nil)
    Select(fun, "apply")
  }

  /**
   * replace arguments in `ain` of type MW[M, _] by a new symbol
   * and construct a function constructing the surrounding
   * map/flatMap wrapper
   *
   * @param ain List of arguments
   * @param first monadic function to call first (map or flatMap)
   */
  def procArgs[M[_]](
    ain:  List[mh_c.Expr[Any]],
    first: String = "flatMap")
  (implicit tt: mh_c.WeakTypeTag[M[_]]) =
      procArgs0[M](ain, first: String, Nil, 0, identity[Tree] _)

  @tailrec
  private def procArgs0[M[_]](
    ain:  List[mh_c.Expr[Any]],
    first: String,
    aout: List[Tree],
    aInd: Int,
    wrap: Tree => Tree)
  (implicit tt: mh_c.WeakTypeTag[M[_]]): (List[Tree], Tree => Tree) = ain match {
    case a :: as => 
      val (out, naInd, nwrap) = procArg[M](a.tree, aInd, wrap, first)
      procArgs0[M](as, "flatMap", out :: aout, naInd, nwrap)
    case Nil => (aout.reverse, wrap)
  }

  def procArg[M[_]](in: Tree, aInd: Int, wrap: Tree => Tree, monFun: String)
  (implicit tt: mh_c.WeakTypeTag[M[_]]) = ckMWType[M](in.tpe) match {
    case Some(vTpe) =>
      val tn = newTermName("arg" + aInd)
      val nwrap = { t: Tree =>
        Apply(SelectMW(in, monFun), lambda(tn -> vTpe)(wrap(t)) :: Nil) }

      (Ident(tn), aInd + 1, nwrap)
    case None =>
      (in, aInd, wrap)
  }

  /**
   * check if the given type is of type MW[T,M] for some T and return
   * Some(typeOf[T]) or None otherwise
   */
  def ckMWType[M[_]](t: Type)(implicit tt: mh_c.WeakTypeTag[M[_]]) = {
    for {
      // Check if t is a MW
      t <- Some(t) if t <:< buildMWType[M]
      // Extract type of value
      TypeRef(_,  _, List(res,_)) = t
    } yield res
  }

  /**
   * build typeOf[MW[_,M]] given WeakTypeTag[M[_]]
   * thanks to Eugene Burmanko
   * http://stackoverflow.com/a/17791973/1149944
   */
  def buildMWType[M[_]](implicit tt: mh_c.WeakTypeTag[M[_]]) = {
    val TypeRef(_, sym, _) = tt.tpe

    val templ = typeOf[MW[_,Hack]]
    templ.substituteSymbols(List(typeOf[Hack[_]].typeSymbol), List(sym))
  }

  /**
   * construct a syntax tree for an anomymous function
   * in context c with given parameters and body
   */
  def lambda(params: (TermName, Type)*)(b: Tree) = {
    import Flag._

    val mods = Modifiers(PARAM)

    val defs =
      for ((tn,tpe) <- params)
        yield ValDef(mods, tn, TypeTree(tpe), EmptyTree)

    Function(defs.toList, b)
  }

}
