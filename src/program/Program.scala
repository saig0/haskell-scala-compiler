package program

import parser.Parser._
import interpreter._
import types._
import compiler.Env._

object Program {

  def run(in: String) = parse(in) match {
    case Success(exp, _) => {
      println("parsed: " + exp)
      TypeChecker.run(exp) match {
        case TypeMismatch(msg) => println("compiler exception: " + msg)
        case _ =>
          println("compiled: ok")
          println("interpreted: " + Interpreter.run(exp))
      }
    }
    case result => println("parser exception: " + result.toString)
  }

  def main(args: Array[String]): Unit = {
    run("(\\ y :: Int -> (\\ x :: Int -> x * y)) 21 2")

    run("(\\ x :: Int -> if(x > 0) then x else 1 ) 6")

    run("let{ fak = rec f :: Int (\\ x :: Int -> if(x > 0) then (x * (f (x - 1))) else 1 )} in fak 5")
  }

}