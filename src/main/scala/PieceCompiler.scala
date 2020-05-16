import PieceParser.ChoiceOfSequence

object PieceCompiler{
  def apply(code:String):Either[FairychessCompilationError,ChoiceOfSequence]={
    for{
      tokens <- Lexer(code)
      moveset <-PieceParser(tokens)
    }yield moveset
  }
}
