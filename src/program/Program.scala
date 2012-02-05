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
  }

}