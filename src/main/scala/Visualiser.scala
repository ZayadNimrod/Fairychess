
import PieceParser.{ChoiceOfSequence, MoveSequence}
import doodle.image._
import doodle.core._
import doodle.image.syntax._
import doodle.java2d._

object Visualiser {

  val scale: Double = 50
  val thickness: Double = 5

  val hyp = Math.sqrt(Math.pow(scale, 2) * 2)

  val blackSquare: Image = Image.rectangle(1, 1).fillColor(Color.black).strokeWidth(0)
  val whiteSquare: Image = Image.rectangle(1, 1).fillColor(Color.rgb(222, 222, 222)).strokeWidth(0)
  val clearSquare: Image = Image.rectangle(1, 1).fillColor(Color.rgba(0, 0, 0, 0)).strokeColor((Color.rgba(0, 0, 0, 0))).strokeWidth(0)
  val startSquare: Image = Image.star(4, 1 / 2.0, 1 / 4.0, Angle(-0.toRadians)).strokeColor(Color.red).strokeWidth(thickness / scale)
  val finalSquare: Image = Image.star(4, 1 / 2.0, 1 / 4.0, Angle(-0.toRadians)).strokeColor(Color.green).strokeWidth(thickness / scale)
  val interSquare: Image = Image.circle(1).strokeColor(Color.lightBlue).strokeWidth(thickness / scale).at(-0.5, -0.5)

  def Show(piece: String, x: Int, y: Int): Unit = {
    PieceCompiler(piece).toOption.map(_ => DrawMoves(_, x, y))

  }


  def DrawMoves(moves: ChoiceOfSequence, x: Int, y: Int) = {

    val centerX: Int = x-Math.floorDiv(x, 2)
    val centerY: Int = y-Math.floorDiv(y, 2)


    //create the board
    val board: Image = Col(Row(blackSquare, whiteSquare, x), Row(whiteSquare, blackSquare, x), y)

    val paths: Image = DrawPaths(moves, -centerX+1, -centerY+1, x - centerX , y - centerY )
    //offset moves then impose on board
    val imposed: Image = board.under(paths.at((centerX-x / 2.0), (centerY-y / 2.0 ))).scale(scale, scale)

    imposed.draw()

  }

  def Row(img: Image, other: Image, rep: Int): Image = {
    rep match {
      case 1 => img
      case _ => Row(other, img, rep - 1).beside(img)
    }
  }

  def Col(img: Image, other: Image, rep: Int): Image = {
    rep match {
      case 1 => img
      case _ => Col(other, img, rep - 1).above(img)
    }
  }

  def DrawPaths(moves: ChoiceOfSequence, minX: Int, minY: Int, maxX: Int, maxY: Int): Image = {
    val drawnMoves: Set[Image] = moves.flatMap(m => DrawPath(m, minX, minY, maxX, maxY, 0, 0))
    drawnMoves.fold(clearSquare) { (z, i) => z.on(i) }

  }

  def DrawPath(move: MoveSequence, minX: Int, minY: Int, maxX: Int, maxY: Int, currentX: Int, currentY: Int): Option[Image] = {
    if (move.isEmpty) {
      Option(startSquare)
    }
    else {
      val atom = move.head

      if (currentX + atom.x > maxX || currentX + atom.x < minX || currentY + atom.y > maxY || currentY + atom.y < minY) {
        None
      } else {
        val subsequent = move.tail

        val line = Image.line(atom.x, atom.y).strokeColor(Color.aliceBlue).strokeWidth(thickness / scale).at((currentX + atom.x / 2.0), (currentY + atom.y / 2.0))

        val restOfMove = DrawPath(subsequent, minX, minY, maxX, maxY, currentX + atom.x, currentY + atom.y)
        val endSymbol = if (subsequent.isEmpty) finalSquare else interSquare
        val lineWithConnectors = line.on(clearSquare.at(currentX, currentY)).on(endSymbol.at((currentX + atom.x), (currentY + atom.y)))
        restOfMove.map(lineWithConnectors.on)
      }
    }
  }
}
