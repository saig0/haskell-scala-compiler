package interpreter

import interpreter.Env._

object Interpreter {

  def value(exp: Exp)(implicit env: Env): Val = {
    exp match {
      case ConstInt(x)   => new ValInt(x)
      case ConstBool(b)  => new ValBool(b)
      case Plus(x, y)    => withInts(value(x), value(y), _ + _, (i: Int) => ValInt(i))
      case Times(x, y)   => withInts(value(x), value(y), _ * _, (i: Int) => ValInt(i))
      case Greater(x, y) => withInts(value(x), value(y), _ > _, (b: Boolean) => ValBool(b))
      case If(c, x, y) =>
        withBool(value(c), { c =>
          if (c) value(x) else value(y)
        })
      case Let(n, x, y) => value(y)(extend(n, value(x)))
      case Ref(n)       => env(n)
      case Abs(n, x)    => new ValFun((v: Val) => value(x)(extend(n, v)))
      case App(x, y) =>
        withFun(value(x), { f =>
          f(value(y))
        })
    }
  }

  def withInts[T](x: Val, y: Val, f: (Int, Int) => T, c: T => Val) =
    withInt(x, { x =>
      withInt(y, { y =>
        c(f(x, y))
      })
    })

  def withInt(x: Val, c: (Int => Val)) =
    x match {
      case ValInt(x) => c(x)
      case _         => throw new IllegalArgumentException("expected Int but found " + x)
    }

  def withBool(b: Val, c: (Boolean => Val)) =
    b match {
      case ValBool(b) => c(b)
      case _          => throw new IllegalArgumentException("expected Bool but found " + b)
    }

  def withFun(f: Val, c: ((Val => Val) => Val)) =
    f match {
      case ValFun(f) => c(f)
      case _         => throw new IllegalArgumentException("expected Function but found " + f)
    }

  def run(exp: Exp) = value(exp)(emptyEnv)

  def main(args: Array[String]): Unit = {

    println(run(Let("x", Plus(ConstInt(1), ConstInt(20)), Times(ConstInt(2), Ref("x")))))

    println(run(Let("f", Abs("x", Times(Ref("x"), Ref("x"))), App(Ref("f"), App(Ref("f"), ConstInt(3))))))

    println(run(Let("x", ConstInt(4), Let("f", Abs("y", Times(Ref("x"), Ref("y"))), Let("x", ConstInt(5), App(Ref("f"), Ref("x")))))))
  }

}