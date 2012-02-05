package interpreter

import interpreter.Env._
import parser.Parser._
import interpreter.Store._
import interpreter.Cps._

object Interpreter {

  def value(exp: Exp)(implicit env: Env, store: Store): CPS = {
    exp match {
      case ConstInt(x)     => new ValInt(x)
      case ConstBool(b)    => new ValBool(b)
      case ConstAddr(addr) => new ValAddr(addr)
      case Plus(x, y)      => withInts(value(x), value(y), _ + _, (i: Int) => ValInt(i))
      case Minus(x, y)     => withInts(value(x), value(y), _ - _, (i: Int) => ValInt(i))
      case Times(x, y)     => withInts(value(x), value(y), _ * _, (i: Int) => ValInt(i))
      case Greater(x, y)   => withInts(value(x), value(y), _ > _, (b: Boolean) => ValBool(b))
      case If(c, x, y) =>
        withBool(value(c), { c =>
          if (c) value(x) else value(y)
        })
      case Let(n, x, y) => value(y)(extend(n, value(x)), store)
      case Ref(n)       => env(n)
      case Abs(n, x)    => new ValFun((v: Val) => value(x)(extend(n, v), store))
      case App(x, y) =>
        withFun(value(x), { f =>
          f(value(y))
        })
      case New(x) => withVal(value(x), { x =>
        insert(x, { addr => ValAddr(addr) })
      })
      case Get(n) => withAddr(value(n), { n =>
        get(n, { x: Val => x })
      })
      case Put(n, x) => withAddr(value(n), { n =>
        withVal(value(x), { x =>
          put(n, x, { u => u })
        })
      })
      case Rec(n, x) =>
        insert(ValError("Rec"), { addr =>
          x match {
            case Abs(x, y) => {
              withVal(value(Abs(x, Let(n, Get(ConstAddr(addr)), y)))(extend(n, ValAddr(addr)), store),
                { v => put(addr, v, { _ => v }) })
            }
            case _ => ValError("expected Abs but found " + x)
          }
        })
    }
  }

  def withInts[T](x: CPS, y: CPS, f: (Int, Int) => T, c: T => Val): CPS =
    withInt(x, { x =>
      withInt(y, { y =>
        c(f(x, y))
      })
    })

  def withInt(x: CPS, c: (Int => CPS)): CPS =
    x match {
      case ValInt(x) => c(x)
      case _         => throw new IllegalArgumentException("expected Int but found " + x)
    }

  def withBool(b: CPS, c: (Boolean => CPS)): CPS =
    b match {
      case ValBool(b) => c(b)
      case _          => throw new IllegalArgumentException("expected Bool but found " + b)
    }

  def withFun(f: CPS, c: ((Val => CPS) => CPS)): CPS =
    f match {
      case ValFun(f) => c(f)
      case _         => throw new IllegalArgumentException("expected Function but found " + f)
    }

  def withVal(x: CPS, c: (Val => CPS)): CPS =
    x match {
      case ValError(msg) => throw new IllegalArgumentException("expected Value but found " + x)
      case _             => c(x)
    }

  def withAddr(x: CPS, c: (Addr => CPS)): CPS =
    x match {
      case ValAddr(addr) => c(addr)
      case _             => throw new IllegalArgumentException("expected Addr but found " + x)
    }

  def run(exp: Exp) = value(exp)(emptyEnv, emptyStore)

  def main(args: Array[String]): Unit = {
    val t = run(Times(Plus(ConstInt(1), ConstInt(1)), Plus(ConstInt(1), ConstInt(20))))

    // Zuweisung
    println(run(Let("x", Plus(ConstInt(1), ConstInt(20)), Times(ConstInt(2), Ref("x")))))
    // einstellige Funktion
    println(run(Let("f", Abs("x", Times(Ref("x"), Ref("x"))), App(Ref("f"), App(Ref("f"), ConstInt(3))))))
    // Variablen Context
    println(run(Let("x", ConstInt(4), Let("f", Abs("y", Times(Ref("x"), Ref("y"))), Let("x", ConstInt(5), App(Ref("f"), Ref("x")))))))
    // mehrstellige Funktion
    println(run(Let("f", Abs("x", Abs("y", Times(Ref("x"), Ref("y")))), App(App(Ref("f"), ConstInt(2)), ConstInt(3)))))
    // Rekusion
    println(run(Let("fak", Rec("f", Abs("x", If(Greater(Ref("x"), ConstInt(0)), Times(Ref("x"), App(Ref("f"), Minus(Ref("x"), ConstInt(1)))), ConstInt(1)))), App(Ref("fak"), ConstInt(5)))))
    // Speicher
    println(run(Let("a", New(ConstInt(42)), Let("t", Put(Ref("a"), ConstInt(21)), Get(Ref("a"))))))
  }

}