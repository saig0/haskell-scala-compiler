package interpreter

sealed trait Exp

case class ConstInt(x: Int) extends Exp

case class ConstBool(b: Boolean) extends Exp

case class Plus(x: Exp, y: Exp) extends Exp

case class Times(x: Exp, y: Exp) extends Exp

case class If(c: Exp, x: Exp, y: Exp) extends Exp

case class Let(n: String, x: Exp, y: Exp) extends Exp

case class Ref(n: String) extends Exp

case class Abs(n: String, x: Exp) extends Exp

case class App(x: Exp, y: Exp) extends Exp
