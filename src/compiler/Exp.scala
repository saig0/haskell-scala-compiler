package compiler

import compiler.Store.Addr
import types.Type

sealed trait Exp

case class ConstInt(x: Int) extends Exp

case class ConstBool(b: Boolean) extends Exp

case class Plus(x: Exp, y: Exp) extends Exp

case class Minus(x: Exp, y: Exp) extends Exp

case class Times(x: Exp, y: Exp) extends Exp

case class Greater(x: Exp, y: Exp) extends Exp

case class If(c: Exp, x: Exp, y: Exp) extends Exp

case class Let(n: String, x: Exp, y: Exp) extends Exp

case class Ref(n: String) extends Exp

case class Abs(n: String, x: Exp) extends Exp

case class App(x: Exp, y: Exp) extends Exp

case class Rec(n: String, x: Exp) extends Exp

case class New(x: Exp) extends Exp

case class Get(n: Exp) extends Exp

case class Put(n: Exp, x: Exp) extends Exp

case class ConstAddr(addr: Addr) extends Exp

case class TypedAbs(n: String, t: Type, x: Exp) extends Exp

case class TypedRec(n: String, t: Type, x: Exp) extends Exp