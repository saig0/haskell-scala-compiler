package interpreter

import scala.collection.mutable.Map

object Env {

  type Env = (String => Val)

  def extend(n: String, x: Val)(implicit env: Env): Env =
    (m: String) => if (m == n) x else env(m)

  def emptyEnv: Env = (n: String) => ValError("variable not found: " + n)
}
