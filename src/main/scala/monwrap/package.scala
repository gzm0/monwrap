import language.higherKinds

package object monwrap {

  def wrap[T,M[_]](m: M[T]): MW[T,M] = new MW(m)
  def unwrap[T,M[_]](mw: MW[T,M]): M[T] = mw.__mon

}
