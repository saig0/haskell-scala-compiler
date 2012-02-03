package parser
import scala.util.parsing.combinator.JavaTokenParsers
import interpreter._

object Parser extends Parser {

  def parse(in: String) = parseAll(program, in)

  def main(args: Array[String]) {
    println(parse("2 * (1 + 20)"))

    println(parse("if true then 42 else 21"))

    println(parse("let {x = 1} in x > y"))

    println(parse("(\\ x -> x * y) 21 2"))
  }
}

class Parser extends JavaTokenParsers {

  val reservedWords = List("if", "then", "else", "let", "in", "true", "false")

  def program: Parser[Exp] = condExp | let | abs | app | exp

  def exp = op | atom

  def atom = number | boolean | variable | parens(program)

  def parens(parser: Parser[Exp]) = "(" ~> parser <~ ")"

  def number = wholeNumber ^^ (i => ConstInt(i.toInt))

  def boolean = ("true" | "false") ^^ (b => ConstBool(b.toBoolean))

  def variable = identifier ^^ (s => Ref(s))

  def identifier = """[a-zA-Z_]\w*""".r

  def op = (dualOp("+", (x, y) => Plus(x, y))
    | dualOp("*", (x, y) => Times(x, y))
    | dualOp(">", (x, y) => Greater(x, y)))

  def dualOp(symbol: String, op: (Exp, Exp) => Exp) =
    atom ~ symbol ~ atom ^^ { case x ~ symbol ~ y => op(x, y) }

  def condExp = "if" ~ atom ~ "then" ~ exp ~ "else" ~ exp ^^
    { case "if" ~ cond ~ "then" ~ x ~ "else" ~ y => If(cond, x, y) }

  def let = "let" ~ "{" ~ identifier ~ "=" ~ program ~ "}" ~ "in" ~ program ^^
    { case "let" ~ "{" ~ name ~ "=" ~ x ~ "}" ~ "in" ~ y => Let(name, x, y) }

  def abs = "\\" ~ identifier ~ "->" ~ program ^^
    { case "\\" ~ n ~ "->" ~ x => Abs(n, x) }

  def app = atom ~ rep1(atom) ^^ { case f ~ args => (f /: args)(App(_, _)) }

}