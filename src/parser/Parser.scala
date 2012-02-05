package parser
import scala.util.parsing.combinator.JavaTokenParsers
import compiler._
import types._

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

  val reservedWords = List(
    "if", "then", "else",
    "let", "in",
    "true", "false",
    "Int", "Boolean",
    "rec",
    "new", "get", "put")

  def program: Parser[Exp] = condExp | let | rec | store | abs | app | exp

  def exp = op | atom

  def atom = number | boolean | variable | parens(program)

  def parens(parser: Parser[Exp]) = "(" ~> parser <~ ")"

  def number = wholeNumber ^^ (i => ConstInt(i.toInt))

  def boolean = ("true" | "false") ^^ (b => ConstBool(b.toBoolean))

  def variable = identifier ^^ (s => Ref(s))

  def identifier = """[a-zA-Z_]\w*""".r

  def op = (dualOp("+", (x, y) => Plus(x, y))
    | dualOp("-", (x, y) => Minus(x, y))
    | dualOp("*", (x, y) => Times(x, y))
    | dualOp(">", (x, y) => Greater(x, y)))

  def dualOp(symbol: String, op: (Exp, Exp) => Exp) =
    atom ~ symbol ~ atom ^^ { case x ~ symbol ~ y => op(x, y) }

  def condExp = "if" ~ atom ~ "then" ~ exp ~ "else" ~ exp ^^
    { case "if" ~ cond ~ "then" ~ x ~ "else" ~ y => If(cond, x, y) }

  def let = "let" ~ "{" ~ identifier ~ "=" ~ program ~ "}" ~ "in" ~ program ^^
    { case "let" ~ "{" ~ name ~ "=" ~ x ~ "}" ~ "in" ~ y => Let(name, x, y) }

  def rec = ("rec" ~ identifier ~ "::" ~ typeName ~ program ^^
    { case "rec" ~ name ~ "::" ~ typeName ~ x => TypedRec(name, typeName, x) }
    | "rec" ~ identifier ~ program ^^ { case "rec" ~ name ~ x => Rec(name, x) })

  def abs = ("\\" ~ identifier ~ "::" ~ typeName ~ "->" ~ program ^^
    { case "\\" ~ n ~ "::" ~ typeName ~ "->" ~ x => TypedAbs(n, typeName, x) }
    | "\\" ~ identifier ~ "->" ~ program ^^ { case "\\" ~ n ~ "->" ~ x => Abs(n, x) })

  def typeName = "Int" ^^ (_ => TypeInt()) | "Boolean" ^^ (_ => TypeBool())

  def app = atom ~ rep1(atom) ^^ { case f ~ args => (f /: args)(App(_, _)) }

  def store = ("new" ~ program ^^ { case "new" ~ value => New(value) }
    | "get" ~ program ^^ { case "get" ~ addr => Get(addr) }
    | "put" ~ program ~ program ^^ { case "put" ~ addr ~ value => Put(addr, value) })

}