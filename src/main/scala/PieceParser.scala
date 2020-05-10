import scala.util.parsing.combinator._
import scala.util.parsing.input._
//lots of this is cribbed from https://enear.github.io/2016/03/31/parser-combinators/


case class FairychessParserError(msg: String) extends FairychessCompilationError


object PieceParser extends Parsers {
  override type Elem = Token

  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = NoPosition

    override def rest: Reader[Token] = new TokenReader(tokens.tail)
  }


  sealed trait MoveAST

  case class ModifiedJump(target: MoveAST, mod: Modifier) extends MoveAST

  case class AtomicJump(x: Int, y: Int) extends MoveAST


  sealed class Modifier()

  case class MirrorX() extends Modifier

  case class MirrorY() extends Modifier

  case class Flip() extends Modifier

  case class Repeat(min: Int, max: Int) extends Modifier

  case class Exponentiate(min: Int, max: Int) extends Modifier

  case class None() extends Modifier

  case class Choice(choices: List[MoveAST]) extends MoveAST

  case class Sequence(head: MoveAST, tail: Sequence) extends MoveAST


  def modifiedJump: Parser[MoveAST] = {


    val normal = baseJump ~ rep1(modifier) ^^ { case atom ~ mods => mods.foldLeft(ModifiedJump(atom, None())) { (z, i) => ModifiedJump(z, i) } }

    val base = baseJump ^^ { case atom => atom }
    normal | base
  }

  def baseJump: Parser[MoveAST] = {
    val brackets = OPENPAREN ~ choice ~ CLOSEPAREN ^^ { case _ ~ j ~ _ => j }
    val atom = OPENJUMP ~ number ~ COMMA ~ number ~ CLOSEJUMP ^^ { case _ ~ NUMBER(x) ~ _ ~ NUMBER(y) ~ _ => AtomicJump(x, y) }
    brackets | atom
  }


  def modifier: Parser[Modifier] = {
    val yMirror = HORIZONTALMIRROR ^^ { _ => MirrorY() }
    val xMirror = VERTICALMIRROR ^^ { _ => MirrorX() }
    val flip = SWAP ^^ { _ => Flip() }
    val rep = repeat ^^ { r => r }
    val exp = exponentiate ^^ { e => e }

    rep | exp | yMirror | xMirror | flip
  }

  def repeat: Parser[Repeat] = {
    val inf = OPENREPITITION ~ RANGE ~ CLOSEREPITITION ^^ { case _ ~ _ ~ _ => Repeat(1, Int.MaxValue) }
    val lower = OPENREPITITION ~ number ~ RANGE ~ CLOSEREPITITION ^^ { case _ ~ NUMBER(l) ~ _ ~ _ => Repeat(l, Int.MaxValue) }
    val upper = OPENREPITITION ~ RANGE ~ number ~ CLOSEREPITITION ^^ { case _ ~ _ ~ NUMBER(u) ~ _ => Repeat(1, u) }
    val both = OPENREPITITION ~ number ~ RANGE ~ number ~ CLOSEREPITITION ^^ { case _ ~ NUMBER(l) ~ _ ~ NUMBER(u) ~ _ => Repeat(l, u) }
    val single = OPENREPITITION ~ number ~ CLOSEREPITITION ^^ { case _ ~ NUMBER(n) ~ _ => Repeat(n, n) }
    lower | upper | inf | both | single
  }

  def exponentiate: Parser[Exponentiate] = {
    val inf = OPENEXPONENT ~ RANGE ~ CLOSEEXPONENT ^^ { case _ ~ _ ~ _ => Exponentiate(1, Int.MaxValue) }
    val lower = OPENEXPONENT ~ number ~ RANGE ~ CLOSEEXPONENT ^^ { case _ ~ NUMBER(l) ~ _ ~ _ => Exponentiate(l, Int.MaxValue) }
    val upper = OPENEXPONENT ~ RANGE ~ number ~ CLOSEEXPONENT ^^ { case _ ~ _ ~ NUMBER(u) ~ _ => Exponentiate(1, u) }
    val both = OPENEXPONENT ~ number ~ RANGE ~ number ~ CLOSEEXPONENT ^^ { case _ ~ NUMBER(l) ~ _ ~ NUMBER(u) ~ _ => Exponentiate(l, u) }
    val single = OPENEXPONENT ~ number ~ CLOSEEXPONENT ^^ { case _ ~ NUMBER(n) ~ _ => Exponentiate(n, n) }
    lower | upper | inf | both | single
  }

  def choice: Parser[MoveAST] = {
    val single = sequence ^^ { case s => s }
    val multi = sequence ~ rep1(COMMA ~ sequence) ^^ { case first ~ subsequent => Choice(first :: (subsequent.map(_._2))) }
    multi | single
  }

  def sequence: Parser[MoveAST] = {
    val single = modifiedJump ^^ { case m => m }
    val multi = modifiedJump ~ rep1(DOT ~ modifiedJump) ^^ { case first ~ rest => Sequence(first, rest.map(_._2).foldRight(Sequence(null, null)) { (i, a) => {
      Sequence(i, a)
    }
    })
    }
    multi | single
  }


  private def number: Parser[NUMBER] = {
    accept("number literal", { case id@NUMBER(name) => id })
  }

  def apply(tokens: Seq[Token]): Either[FairychessParserError, MoveAST] = {
    val reader = new TokenReader(tokens)
    choice(reader) match {
      case NoSuccess(msg, next) => Left(FairychessParserError(msg))
      case Success(result, next) => Right(result)
    }
  }


}