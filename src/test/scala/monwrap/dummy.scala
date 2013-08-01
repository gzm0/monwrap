package monwrap

object Test extends App {

  val w = wrap { Some("asdf") }
  val i = wrap { Some(1) }

  val j = wrap { Some(2) }

  println(w.substring(3))
  println(w.substring(i))

  println(i - j)

}
