package types
import compiler._
import compiler.Env._
import compiler.Store._
import types._
import scala.collection.mutable.Map

object TypeChecker {

  def emptyTypedEnv: Env[Type] = (n: String) => TypeMismatch("variable not found: " + n)

  def emptyTypedStore: Store[Type] = Map[Addr, Type]()

  def value(exp: Exp)(implicit env: Env[Type], store: Store[Type]): Type =
    exp match {
      case ConstInt(_)     => TypeInt()
      case ConstBool(_)    => TypeBool()
      case ConstAddr(addr) => get(addr, { t: Type => TypeAddr(t) })
      case Plus(x, y)      => withInts(value(x), value(y), () => TypeInt())
      case Minus(x, y)     => withInts(value(x), value(y), () => TypeInt())
      case Times(x, y)     => withInts(value(x), value(y), () => TypeInt())
      case Greater(x, y)   => withInts(value(x), value(y), () => TypeBool())
      case If(c, x, y) => withBool(value(c), { () =>
        withType(value(x), { t1 =>
          withType(value(y), { t2 =>
            if (t1 == t2) t1
            else TypeMismatch("expected " + t1 + " but found " + t2)
          })
        })
      })
      case Ref(n)       => env(n)
      case Let(n, x, y) => withType(value(x), { t => value(y)(extend(n, t), store) })
      case Abs(n, x)    => TypeMismatch("expected TypeAbs but found " + exp)
      case TypedAbs(n, t1, x) => withType(value(x)(extend(n, t1), store), { t2 =>
        TypeFun(t1, t2)
      })
      case App(f, x) => withFun(value(f), { (t1, t2) =>
        withType(value(x), { t3 =>
          if (t1 == t3) t2
          else TypeMismatch("expected " + t1 + " but found " + t3)
        })
      })
      case New(x) => withType(value(x), { x =>
        insert(x, { addr => TypeAddr(x) })
      })
      case Get(n) => withAddr(value(n), { t => t })
      case Put(n, x) => withAddr(value(n), { t =>
        withType(value(x), { typeX =>
          if (t == typeX) TypeUnit()
          else TypeMismatch("expected " + t + " but found " + typeX)
        })
      })
      case Rec(n, x) => TypeMismatch("expected TypeRec but found " + exp)
      case TypedRec(n, t, x) => withType(value(x)(extend(n, new TypeFun(t, t)), store),
        { t2 => t2 })
    }

  def withInts(x: Type, y: Type, c: () => Type) =
    withInt(x, { () =>
      withInt(y, { () => c() })
    })

  def withInt(x: Type, c: () => Type) =
    x match {
      case TypeInt() => c()
      case _         => TypeMismatch("expected Int but found " + x)
    }

  def withBool(b: Type, c: () => Type) =
    b match {
      case TypeBool() => c()
      case _          => TypeMismatch("expected Bool but found " + b)
    }

  def withType(t: Type, c: Type => Type) =
    t match {
      case TypeMismatch(msg) => TypeMismatch("expected Type but found " + t)
      case _                 => c(t)
    }

  def withFun(f: Type, c: (Type, Type) => Type) =
    f match {
      case TypeFun(t1, t2) => c(t1, t2)
      case _               => TypeMismatch("expected Function but found " + f)
    }

  def withAddr(x: Type, c: (Type => Type)) =
    x match {
      case TypeAddr(addr) => c(addr)
      case _              => TypeMismatch("expected Addr but found " + x)
    }

  def run(exp: Exp) = value(exp)(emptyTypedEnv, emptyTypedStore)

  def main(args: Array[String]): Unit = {
    println(run(Plus(ConstInt(1), ConstInt(20))))
    println(run(If(ConstBool(true), ConstInt(1), ConstInt(20))))
    println(run(Let("x", ConstInt(1), Plus(ConstInt(1), Ref("x")))))
    println(run(Let("f", TypedAbs("x", TypeInt(), Times(Ref("x"), Ref("x"))), App(Ref("f"), ConstInt(3)))))
    println(run(Let("x", New(ConstInt(42)), Put(Ref("x"), ConstInt(21)))))
    println(run(Let("fak", TypedRec("f", TypeInt(), TypedAbs("x", TypeInt(), If(Greater(Ref("x"), ConstInt(0)), Times(Ref("x"), App(Ref("f"), Minus(Ref("x"), ConstInt(1)))), ConstInt(1)))), App(Ref("fak"), ConstInt(5)))))

  }

}