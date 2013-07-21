package monwrap

object Test extends App {

  val w = new MW[String,Option](Some("asdf"))
  val i = new MW[Int,Option](Some(1))

  println(w.substring(i))

}
