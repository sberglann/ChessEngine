
/**
  * Created by Sigurd on 03.01.2017.
  */
case class Board(position: List[Piece], info: List[Boolean], enPassantCol: Int) {
  /**
    * info is a list of booleans representing board information.
    * 0: white is checked
    * 1: black is checked
    * 2: no earlier move prohibits WHITE from castling west
    * 3: no earlier move prohibits BLACK from castling west
    * 4: no earlier move prohibits WHITE from castling east
    * 5: no earlier move prohibits BLACK from castling east
    *
    * enPassantCol: Contains the column number of where a pawn can capture with en passant. If last move was not a
    * double-step pawn move, it is simply set to -1.
    */

  lazy val whiteKingPos = position.indexWhere(piece => piece.pieceType == 'K' && piece.color == 'W')
  lazy val blackKingPos = position.indexWhere(piece => piece.pieceType == 'K' && piece.color == 'B')

  def row(i: Int) = i / 8
  def col(i: Int) = i % 8
  def validIndex(i: Int) = i >= 0 && i < 64
  def validRow(pos: Int, delta: Int, limit: Int) = math.abs(row(pos) - row(pos + delta)) <= limit
  def validCol(pos: Int, delta: Int, limit: Int) = math.abs(col(pos) - col(pos + delta)) <= limit
  def emptySquare(pos: Int) = position(pos).pieceType == 'E'

  def deletePiece(pos: Int) = this.copy(position = position.updated(pos, Piece("EE")))

  lazy val prettyPieces = Map("EE" -> " … ", "WK" -> " ♔ ", "WQ" -> " ♕ ", "WR" -> " ♖ ",
    "WB" -> " ♗ ", "WN" -> " ♘ ", "WP" -> " ♙ ",
    "BK" -> " ♚ ", "BQ" -> " ♛ ", "BR" -> " ♜ ", "BB" -> " ♝ ",
    "BN" -> " ♞ ", "BP" -> " ♟ ")

  def prettyBoard = {
    (for (i <- position.indices) yield {
      if (i % 8 == 0) s"\n ${8 - i/8} ║${prettyPieces(position(i).color.toString + position(i).pieceType.toString)}"
      else prettyPieces(position(i).color.toString + position(i).pieceType.toString)
    }).foldLeft("")(_ + _) ++ "\n   ╚════════════════════════\n      a   b   c   d    e   f   g    h"
  }

  def changedBoard(pos: Int, newPos: Int): Board = {
    //TODO: Add checks to info list, or change list.

    val newEnPassantCol = if (position(pos).pieceType == 'P' && math.abs(row(pos) - row(newPos)) == 2) col(pos) else -1

    //Checks if castle or king is moved. If this is the case, castling cannot be done, and the info-list is changed
    val newInfo =
      if (pos == whiteKingPos)      info.updated(2, false).updated(4, false)
      else if (pos == blackKingPos) info.updated(3, false).updated(5, false)
      else if (pos == 0)            info.updated(3, false)
      else if (pos == 7)            info.updated(5, false)
      else if (pos == 56)           info.updated(2, false)
      else if (pos == 63)           info.updated(4, false)
      else                          info

    this.copy(position = position
      .updated(pos, Piece("EE"))
      .updated(newPos, this.position(pos)), info = newInfo, enPassantCol = newEnPassantCol)
  }

  def generateSuccessor(color: Char) = {
    val moves = Moves(this)
    position
      .zipWithIndex
      .filter{ case (piece, index) => piece.color == color }
      .flatMap{ case (piece, index) => moves.getMovesAt(index).map(move => changedBoard(index, move))} ++
      moves.castleMoves(color)
  }

}
