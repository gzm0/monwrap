package object monwrap {

  def unwrap[T](mw: MW[T]): Option[T] = mw.__mon

}
