package program

import parser.Parser._
import interpreter._
import interpreter.Env._

object Program {

  def run(in: String) = parse(in) match {
    case Success(exp, _) => {
      println("parsed: " + exp)
      try {
        println("interpreted: " + Interpreter.run(exp))
      } catch {
        case e: Exception => println("interpreter exception: " + e)
      }
    }
    case result => println("parser exception: " + result.toString)
  }

  def main(args: Array[String]): Unit = {

    run("(\\ y -> (\\ x -> x * y)) 21 2")
  }

}