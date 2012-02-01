package interpreter

sealed trait Val

case class ValInt(x: Int) extends Val

case class ValBool(b: Boolean) extends Val

case class ValError(msg: String) extends Val

case class ValFun(f: Val => Val) extends Val