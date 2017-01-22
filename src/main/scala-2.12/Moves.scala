/**
  * Created by Sigurd on 03.01.2017.
  */
import scala.annotation.tailrec
import scala.collection.GenTraversableOnce

case class Moves(board: Board) {

  def row(i: Int) = i / 8
  def col(i: Int) = i % 8

  // Returns true if color is checked
  def check(color: Char, newBoard: Board) = {
    val kingPosition = if (color == 'W') newBoard.whiteKingPos else if (color == 'B') newBoard.blackKingPos else -1
    newBoard.position
      .zipWithIndex
      .filter{case (piece, pos) => piece.color != 'E' && piece.color != color}
      .exists{case (piece, pos) => Moves(newBoard).covers(pos, kingPosition)}
  }

  def kingMoves(pos: Int): List[Int] = {
    List(-9, -8, -7, -1, 1, 7, 8, 9)
      .filter(delta => board.validIndex(pos + delta) && board.validCol(pos, delta, 1))
      .map(delta => pos + delta)
      .filter(newPos => board.position(newPos).color != board.position(pos).color)
  }

  def queenMoves(pos: Int): List[Int] = rookMoves(pos) ++ bishopMoves(pos)

  def rookMoves(pos: Int): List[Int] = {
    // TODO: Add test to check if path is blocked for each direction, reducing number of calls to slide()
    (slide(pos, 0, -8 to -56 by -8, Nil) ++ slide(pos, 0, 8 to 56 by 8, Nil) ++
      slide(pos, 0, -1 to -8 by -1, Nil) ++ slide(pos, 0, 1 to 8, Nil))
      .map(delta => pos + delta)
  }

  def bishopMoves(pos: Int): List[Int] = {
    // TODO: Add test to check if path is blocked for each direction, reducing number of calls to slide()
    (slide(pos, 0, -7 to -49 by -7, Nil) ++ slide(pos, 0, 7 to 49 by 7, Nil) ++
      slide(pos, 0, -9 to -63 by -9, Nil) ++ slide(pos, 0, 9 to 63 by 9, Nil))
      .map(delta => pos + delta)
  }

  def knightMoves(pos: Int): List[Int] = {
    val delta = List(-17, -15, -10, -6, 6, 10, 15, 17)
    delta
      .filter(delta => board.validIndex(delta + pos) && board.validRow(pos, delta, 2) && board.validCol(pos, delta, 2))
      .map(delta => pos + delta)
      .filter(newPos => board.position(newPos).pieceType == 'E' || board.position(newPos).color != board.position(pos).color)

  }


  def pawnMoves(pos: Int): List[Int] = {
    var delta = scala.collection.mutable.Buffer[Int]()
    if (board.position(pos).color == 'W') {
      if (row(pos) == 3 && math.abs(board.enPassantCol - col(pos)) == 1) delta += board.enPassantCol - col(pos) - 8
      if (row(pos) > 0){
        if (pos >= 8 && board.emptySquare(pos - 8)) delta += -8
        if (row(pos) == 6 && board.emptySquare(pos - 16) && board.emptySquare(pos - 8)) delta += -16
        if (pos >= 8 && !board.emptySquare(pos - 9) && col(pos) != 0 && board.position(pos-9).color == 'B') delta += -9
        if (pos >= 8 && !board.emptySquare(pos - 7) && col(pos) != 7 && board.position(pos-7).color == 'B') delta += -7
      }
    }
    else {
      if (row(pos) == 4 && math.abs(board.enPassantCol - col(pos)) == 1) delta += 8 - col(pos) + board.enPassantCol
      if (row(pos) < 7){
        if (board.emptySquare(pos + 8)) delta += 8
        if (row(pos) == 1 && board.emptySquare(pos + 16) && board.emptySquare(pos + 8)) delta += 16
        if (!board.emptySquare(pos + 9) && col(pos) != 7 && board.position(pos + 9).color == 'W') delta += 9
        if (!board.emptySquare(pos + 7) && col(pos) != 0 && board.position(pos + 7).color == 'W') delta += 7
      }
    }
    delta.map(delta => pos + delta).toList
  }

  // Generic method for generating moves for sliding pieces -- queen, bishops and rooks
  @tailrec
  final def slide(pos: Int, prevDelta: Int, delta: Range, result: List[Int]): List[Int] = {
    if (delta.nonEmpty
      && board.validIndex(pos + delta.head)
      && board.validCol(pos + prevDelta, delta.head - prevDelta, 1)
      && board.position(pos + delta.head).color != board.position(pos).color){
      if (board.position(pos + delta.head).color != 'E') result ++ Seq(delta.head)
      else slide(pos, delta.head, delta.tail, result ++ Seq(delta.head))
    }
    else result
  }

  //Checks if king can castle to direction. Fields between king and rook must be empty, and cannot be attacked by enemy.
  //The info list contains false if the relevant rook or the king has moved.
  def castleWestWhite = {
    if (board.info(2) && Seq(57, 58, 59).forall(pos => board.emptySquare(pos)) &&
      !Seq(58, 59, 60).exists(pos => fieldAttacked(pos, 'B'))){
      List(board.changedBoard(56, 59).changedBoard(60, 58))
    } else Nil
  }

  def castleEastWhite = {
    if (board.info(4) && Seq(61, 62).forall(pos => board.emptySquare(pos)) &&
      !Seq(60, 61, 62).exists(pos => fieldAttacked(pos, 'B'))){
        List(board.changedBoard(63, 61).changedBoard(60, 62))
      } else Nil
  }

  def castleWestBlack = {
    if (board.info(3) && Seq(1, 2, 3).forall(pos => board.emptySquare(pos)) &&
      !Seq(2, 3, 4).exists(pos => fieldAttacked(pos, 'W'))){
      List(board.changedBoard(7, 5).changedBoard(4, 6))
    } else Nil
  }

  def castleEastBlack = {
    if (board.info(5) && Seq(5, 6).forall(pos => board.emptySquare(pos)) &&
      !Seq(4, 5, 6).exists(pos => fieldAttacked(pos, 'W'))){
      List(board.changedBoard(0, 3).changedBoard(4, 2))
    } else Nil
  }

  def castleMoves(color: Char): List[Board] = {
    if (color == 'W') castleWestWhite ++ castleEastWhite
    else castleWestBlack ++ castleEastBlack
  }

  def getMovesAt(pos: Int) = {
    movesFromType(pos).filter(newPos => !check(board.position(pos).color, board.changedBoard(pos, newPos)))
  }

  def movesFromType(pos: Int): List[Int] = board.position(pos).pieceType match{
    case 'K' => kingMoves(pos)
    case 'Q' => queenMoves(pos)
    case 'R' => rookMoves(pos)
    case 'B' => bishopMoves(pos)
    case 'N' => knightMoves(pos)
    case 'P' => pawnMoves(pos)
    case 'E' => Nil
  }

  def covers(pos: Int, coverIndex: Int): Boolean = movesFromType(pos).contains(coverIndex)

  //Returns true if field is attacked by color
  def fieldAttacked(coverIndex: Int, color: Char) = board.position
    .zipWithIndex
    .filter{ case (piece, index) => piece.color == color}
    .exists{ case (piece, index) => covers(index, coverIndex)}
}

