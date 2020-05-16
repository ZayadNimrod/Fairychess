import scala.collection.SortedSet
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

  type ChoiceOfSequence = Set[MoveSequence]
  type MoveSequence = IndexedSeq[AtomicJump]

  sealed trait MoveAST {
    //this returns the moveAST in "choice of sequences" form
    def simplify(): ChoiceOfSequence
  }

  case class ModifiedJump(target: MoveAST, mod: Modifier) extends MoveAST {
    override def simplify() = mod.modify(target.simplify())
  }

  case class AtomicJump(x: Int, y: Int) extends MoveAST {
    override def simplify() = Set(IndexedSeq(this))
  }


  sealed abstract class Modifier() {
    //returns the move given with the modifier applied to it
    def modify(move: ChoiceOfSequence): ChoiceOfSequence


  }

  case class MirrorX() extends Modifier {
    override def modify(move: ChoiceOfSequence) = {
      move.map(_.map((i: AtomicJump) => AtomicJump(-i.x, i.y)))++move
    }
  }

  case class MirrorY() extends Modifier {
    override def modify(move: ChoiceOfSequence) = {
      move.map(_.map((i: AtomicJump) => AtomicJump(i.x, -i.y)))++move
    }
  }

  case class Flip() extends Modifier {
    override def modify(move: ChoiceOfSequence) = {
      move.map(_.map((i: AtomicJump) => AtomicJump(-i.y, i.x)))++move
    }
  }

  case class Repeat(min: Int, max: Int) extends Modifier {
    override def modify(move: ChoiceOfSequence): ChoiceOfSequence = {
      (for (i <- min to max) yield rep(move, i)).toSet.flatten
    }

    def rep(move: ChoiceOfSequence, r: Int) = {
      var ret:ChoiceOfSequence = Set[MoveSequence]()
      for (i: MoveSequence <- move) {
        var cur: MoveSequence = IndexedSeq[AtomicJump]()
        for (j <- 1 to r) {
          cur ++= i
        }
        ret+=cur
      }
      ret
    }
  }

  case class Exponentiate(min: Int, max: Int) extends Modifier {
    override def modify(move: ChoiceOfSequence): ChoiceOfSequence = {
      ((for (i <- min to max) yield multiply(move, i)).toSet).flatten
    }

    def multiply(moves: PieceParser.ChoiceOfSequence, i: Int): ChoiceOfSequence = {
      if (i == 1) {moves}
      else {
        val tail = multiply(moves, i - 1)
        //moves.map((h:MoveSequence) => tail.map((t:MoveSequence)=>h++t)).fold(Set()){(z,i)=>z++i}//.flatten
        moves.flatMap((h: MoveSequence) => {
          tail.map((t: MoveSequence) => h ++ t)
        })
      }
    }
  }

  case class None() extends Modifier {
    override def modify(move: ChoiceOfSequence): ChoiceOfSequence = move
  }

  case class Choice(choices: List[MoveAST]) extends MoveAST {
    override def simplify(): ChoiceOfSequence = {
      val sChoices = choices.map(x => x.simplify())
      //combine the simplified choices
      sChoices.fold(Set[MoveSequence]()) { (z, i) => z ++ i }
    }
  }

  case class Sequence(head: MoveAST, tail: MoveAST) extends MoveAST {
    override def simplify(): ChoiceOfSequence = {
      val sHead: ChoiceOfSequence = head.simplify()
      val sTail: ChoiceOfSequence = tail.simplify()

      sHead.flatMap((h: MoveSequence) => {
        sTail.map((t: MoveSequence) => h ++ t)
      })

    }
  }


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
    val yMirror = VERTICALMIRROR ^^ { _ => MirrorY() }
    val xMirror = HORIZONTALMIRROR ^^ { _ => MirrorX() }
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
    //TODO modify this so that tail node is not the null sequence
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

  def apply(tokens: Seq[Token]): Either[FairychessParserError, ChoiceOfSequence] = {
    val reader = new TokenReader(tokens)
    choice(reader) match {
      case NoSuccess(msg, next) => Left(FairychessParserError(msg))
      case Success(result, next) => Right(result.simplify())
    }
  }


}