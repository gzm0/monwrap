package monwrap

object Test extends App {

  val w = wrap { Some("asdf") }
  val i = wrap { Some(1) }

  println(w.substring(i))

}
