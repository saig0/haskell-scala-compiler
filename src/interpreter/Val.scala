package interpreter
import compiler.Store.Addr

sealed trait Val

case class ValInt(x: Int) extends Val

case class ValBool(b: Boolean) extends Val

case class ValError(msg: String) extends Val

case class ValFun(f: Val => Val) extends Val

case class ValAddr(a: Addr) extends Val

case class ValUnit extends Val