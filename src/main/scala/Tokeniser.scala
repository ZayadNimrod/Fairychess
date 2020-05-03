import scala.util.parsing.combinator._

//lots of this is cribbed from https://enear.github.io/2016/03/31/parser-combinators/
sealed trait Token


case class NUMBER(str: Int) extends Token

case object OPENPAREN extends Token

case object CLOSEPAREN extends Token

case object OPENBRACKET extends Token

case object CLOSEBRACKET extends Token

case object COMMA extends Token

case object VERTICALMIRROR extends Token

case object HORIZONTALMIRROR extends Token

case object XYMIRROR extends Token

case object OPENBRACE extends Token

case object CLOSEBRACE extends Token

case object RANGE extends Token

case class MACRO(name: String) extends Token


  object Lexer extends RegexParsers {
    override def skipWhitespace = true

    override val whiteSpace = "[ \t\r\f\n]+".r

    //define the individual rules

    def number: Parser[NUMBER] = {
      """(0|[1-9]\d*)""".r ^^ { str => NUMBER(str.toInt) }
    }

    def macroexp: Parser[MACRO] = {
      "[a-zA-Z+*]".r ^^ { str => MACRO(str) }
    }

    def openparen = "(" ^^ (_ => OPENPAREN)

    def closeparen = ")" ^^ (_ => CLOSEPAREN)

    def openbracket = "[" ^^ (_ => OPENBRACKET)

    def closebracket = "]" ^^ (_ => CLOSEBRACKET)

    def comma = "," ^^ (_ => COMMA)

    def mirrorvertical = "-" ^^ (_ => VERTICALMIRROR)

    def mirrorhorizontal = "|" ^^ (_ => HORIZONTALMIRROR)

    def flip = "/" ^^ (_ => XYMIRROR)

    def openbrace = "{" ^^ (_ => OPENBRACE)

    def closebrace = "}" ^^ (_ => CLOSEBRACE)

    def range = ".." ^^ (_ => RANGE)

    //combine the rules
    def tokens: Parser[List[Token]] = {
      phrase(rep1(openparen | closeparen | openbracket | closebracket | comma | mirrorvertical | mirrorhorizontal
        | flip | openbrace | closebrace | range | number | macroexp)) ^^ { rawTokens =>
        processMacros(rawTokens)
      }
    }

    //TODO this must be populated when parsing a macro
    var macros: Map[String, List[Token]] = Map()

    //post-process step to apply macros
    //TODO does not yet allow for user-defined macros
    private def processMacros(tokens: List[Token]): List[Token] = {
      tokens.headOption match {
        //apply the built-in macros
        case Some(MACRO("+")) => processMacros(HORIZONTALMIRROR :: VERTICALMIRROR :: tokens.tail)
        case Some(MACRO("*")) => processMacros(XYMIRROR :: MACRO("+") :: tokens.tail)
        //if there is a macro, prepend its tokenised values and continue
        case Some(MACRO(value)) =>
          processMacros(macros(value) ++ tokens.tail)

        // other tokens are ignored
        case Some(token) =>
          token :: processMacros(tokens.tail)
        case None =>
          Nil

      }
    }
    trait FairychessCompilationError
    case class LexerError(msg: String) extends FairychessCompilationError


    def apply(code: String): Either[LexerError, List[Token]] = {
      parse(tokens, code) match {
        case NoSuccess(msg, next) => Left(LexerError(msg))
        case Success(result, next) => Right(result)
      }
    }




}
