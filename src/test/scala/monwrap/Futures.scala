package monwrap

import Predef.{any2stringadd => _}
import Predef.println

import scala.concurrent._

object Futures extends App {

  import ExecutionContext.Implicits.global

  def wf[A](x: => A) = wrap { future { x } }

  val x = wf { 1.0 }
  val y = wf { 2.0 }
  val z = wf { 3.0 }

  val tmp = ((x - y) / z) * x

  // The + here requires masking any2stringadd
  val res = tmp * tmp / y + x

  unwrap { res } foreach { println _ }

}
