package object monwrap {

  def unwrap[T,M[V]](mw: MW[T,M]): M[T] = mw.__mon

}
