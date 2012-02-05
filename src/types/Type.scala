package types

sealed trait Type

case class TypeInt extends Type

case class TypeBool extends Type

case class TypeAddr(ref: Type) extends Type

case class TypeFun(x: Type, y: Type) extends Type

case class TypeVar(n: String) extends Type

case class TypeAbs(n: String, x: Type) extends Type

case class TypeMismatch(msg: String) extends Type

case class TypeUnit extends Type