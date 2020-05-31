
import PieceParser.{ChoiceOfSequence, MoveSequence}
import doodle.image._
import doodle.core._
import doodle.image.syntax._
import doodle.java2d._

object Visualiser {

  val scale = 50
  val thickness = 5

  val hyp = Math.sqrt(Math.pow(scale,2)*2)

  val blackSquare: Image = Image.rectangle(scale, scale).fillColor(Color.black)
  val whiteSquare: Image = Image.rectangle(scale, scale).fillColor(Color.rgb(222, 222, 222))
  val clearSquare: Image = Image.rectangle(scale, scale).fillColor(Color.rgba(0, 0, 0, 0)).strokeColor((Color.rgba(0, 0, 0, 0)))
  val startSquare: Image = Image.star(5, scale/ 2.0, scale / 4.0, Angle(-195.toRadians)).strokeColor(Color.red).strokeWidth(thickness)
  val finalSquare: Image = Image.star(5, scale / 2.0, scale / 4.0, Angle(-195.toRadians)).strokeColor(Color.green).strokeWidth(thickness)
  val interSquare: Image = Image.circle(scale ).strokeColor(Color.lightBlue).strokeWidth(thickness)

  def Show(piece: String, x: Int, y: Int): Unit = {
    PieceCompiler(piece).toOption.map(_ => DrawMoves(_, x, y))

  }


  def DrawMoves(moves: ChoiceOfSequence, x: Int, y: Int) = {

    val centerX: Int = x / 2
    val centerY: Int = y / 2


    //create the board
    val board: Image = Col(Row(blackSquare, whiteSquare, x), Row(whiteSquare, blackSquare, x), y)

    val paths: Image = DrawPaths(moves, -centerX, -centerY, x - centerX - 1, y - centerY - 1)
    //offset moves then impose on board
    val imposed: Image = board.under(paths.at((centerX - x / 2.0+0.5) * scale, (centerY - y / 2.0+0.5) * scale))

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

        val line = Image.line(atom.x * scale, atom.y * scale).strokeColor(Color.aliceBlue).strokeWidth(thickness).at((currentX + atom.x / 2.0) * scale, (currentY + atom.y / 2.0) * scale)

        val restOfMove = DrawPath(subsequent, minX, minY, maxX, maxY, currentX + atom.x, currentY + atom.y)
        val endSymbol = if (subsequent.isEmpty) finalSquare else interSquare
        val lineWithConnectors = line.on(clearSquare.at(currentX * scale, currentY * scale)).on(endSymbol.at((currentX + atom.x) * scale, (currentY + atom.y) * scale))
        restOfMove.map(lineWithConnectors.on)
      }
    }
  }
}
