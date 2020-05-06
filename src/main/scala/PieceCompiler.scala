import PieceParser.MoveAST

object PieceCompiler {
  def apply(code: String): Either[FairychessCompilationError, MoveAST] = {
    for {
      tokens <- Lexer(code)
      ast <- PieceParser(tokens)
    } yield ast
  }
}
