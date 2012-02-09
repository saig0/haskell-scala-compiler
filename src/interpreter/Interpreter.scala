package interpreter

import interpreter.Env._
import parser.Parser._
import monad.Action._
import monad.Action.ActionMonad._
import monad._

object Interpreter {

  def value(exp: Exp)(implicit env: Env): Action[Val] = {
    exp match {
      case ConstInt(x)   => unit(new ValInt(x))
      case ConstBool(b)  => unit(new ValBool(b))
      case Plus(x, y)    => withInts(value(x), value(y), _ + _, (i: Int) => unit(ValInt(i)))
      case Times(x, y)   => withInts(value(x), value(y), _ * _, (i: Int) => unit(ValInt(i)))
      case Greater(x, y) => withInts(value(x), value(y), _ > _, (b: Boolean) => unit(ValBool(b)))
      case If(c, x, y) =>
        withBool(value(c), { c =>
          if (c) value(x) else value(y)
        })
      case Let(n, x, y) => withVal(value(x), { v => value(y)(extend(n, v)) })
      case Ref(n)       => unit(env(n))
      case Abs(n, x)    => unit(new ValFun((v: Val) => value(x)(extend(n, v))))
      case App(x, y) =>
        withFun(value(x), { f =>
          withVal(value(y), { v => f(v) })
        })
    }
  }

  def withInts[T](x: Action[Val], y: Action[Val], f: (Int, Int) => T, c: T => Action[Val]) =
    withInt(x, { x =>
      withInt(y, { y =>
        c(f(x, y))
      })
    })

  def withInt(x: Action[Val], c: (Int => Action[Val])): Action[Val] =
    x >>= { v =>
      v match {
        case ValInt(x) => c(x)
        case _         => throw new IllegalArgumentException("expected Int but found " + x)
      }
    }

  def withBool(b: Action[Val], c: (Boolean => Action[Val])) =
    b >>= { b =>
      b match {
        case ValBool(b) => c(b)
        case _          => throw new IllegalArgumentException("expected Bool but found " + b)
      }
    }

  def withFun(f: Action[Val], c: ((Val => Action[Val]) => Action[Val])) =
    f >>= { f =>
      f match {
        case ValFun(f) => c(f)
        case _         => throw new IllegalArgumentException("expected Function but found " + f)
      }
    }

  def withVal(a: Action[Val], c: (Val => Action[Val])): Action[Val] =
    a >>= { a =>
      a match {
        case ValError(msg) => unit(ValError(msg))
        case _             => c(a)
      }
    }

  def run(exp: Exp) = Action.run(value(exp)(emptyEnv))

  def main(args: Array[String]): Unit = {
    // Zuweisung
    println(run(Let("x", Plus(ConstInt(1), ConstInt(20)), Times(ConstInt(2), Ref("x")))))
    // einstellige Funktion
    println(run(Let("f", Abs("x", Times(Ref("x"), Ref("x"))), App(Ref("f"), App(Ref("f"), ConstInt(3))))))
    // Variablen Context
    println(run(Let("x", ConstInt(4), Let("f", Abs("y", Times(Ref("x"), Ref("y"))), Let("x", ConstInt(5), App(Ref("f"), Ref("x")))))))
    // mehrstellige Funktion
    println(run(Let("f", Abs("x", Abs("y", Times(Ref("x"), Ref("y")))), App(App(Ref("f"), ConstInt(2)), ConstInt(3)))))
    // Rekusion

  }

}