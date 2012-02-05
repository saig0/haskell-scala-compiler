package compiler

import scala.collection.mutable.Map
import types._

object Env {

  type Env[T] = (String => T)

  def extend[T](n: String, x: T)(implicit env: Env[T]): Env[T] =
    (m: String) => if (m == n) x else env(m)

}
