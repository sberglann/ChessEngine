/**
  * Created by Sigurd on 13.01.2017.
  */
case class Evaluation(board: Board) {

  val numWhitePawns = board.position.count(piece => piece.prop == "WP")
  val numBlackPawns = board.position.count(piece => piece.prop == "BP")

  def totalHeuristics(): Double = {
    //material + pawn
    positionValues + knight + bishop + rook + mobility
  }

  def material: Double = board.position.foldLeft(0.0)(_ + _.value)

  def positionValues = {
    val knightValues = Array(
        -.50, -.40, -.30, -.30, -.30, -.30, -.40, -.50,
        -.40, -.20, 0, 0, 0, 0, -.20, -.40,
        -.30, 0, .10, .15, .15, .10, 0, -.30,
        -.30, .05, .15, .20, .20, .15, .05, -.30,
        -.30, 0, .15, .20, .20, .15, 0, -.30,
        -.30, .05, .10, .15, .15, .10, .05, -.30,
        -.40, -.20, 0, .05, .05, 0, -.20, -.40,
        -.50, -.40, -.30, -.30, -.30, -.30, -.40, -.50)

    val bishopValues = Array(
      -.20, -.10, -.10, -.10, -.10, -.10, -.10, -.20,
      -.10,   0,    0,    0,    0,    0,    0,  -.10,
      -.10,   0,  .05,  .10,  .10,  .05,    0,  -.10,
      -.10, .05,  .05,  .10,  .10,  .05,  .05,  -.10,
      -.10,   0,  .10,  .10,  .10,  .10,    0,  -.10,
      -.10, .10,  .10,  .10,  .10,  .10,  .10,  -.10,
      -.10, .05,    0,    0,    0,    0,  .05,  -.10,
      -.20, -.10, -.10, -.10, -.10, -.10, -.10, -.20)

    val rookValues = Array(
      0,  0,   0,   0,   0,   0,   0,   0,
      .05, .10, .10, .10, .10, .10, .10, .05,
      -.05,  0,   0,   0,   0,   0,   0, -.05,
      -.05,  0,   0,   0,   0,   0,   0, -.05,
      -.05,  0,   0,   0,   0,   0,   0, -.05,
      -.05,  0,   0,   0,   0,   0,   0, -.05,
      -.05,  0,   0,   0,   0,   0,   0, -.05,
      0,  0,   0, .05, .05,   0,   0,  0
    )

    val queenValues = Array(
      -.20, -.10, -.10, -.05, -.05, -.10, -.10, -.20,
      -.10,  0,  0,  0,  0,  0,  0, -.10,
      -.10,  0,  .05,  .05,  .05,  .05,  0, -.10,
      -.05,  0,  .05,  .05,  .05,  .05,  0, -.05,
      0,  0,  .05,  .05,  .05,  .05,  0, -.05,
      -.10,  .05,  .05,  .05,  .05,  .05,  0, -.10,
      -.10,  0,  .05,  0,  0,  0,  0, -.10,
      -.20, -.10, -.10, -.05, -.05, -.10, -.10, -.20
    )

    val kingValuesEarly = Array(
      -.30, -.40, -.40, -.50, -.50, -.40, -.40, -.30,
      -.30, -.40, -.40, -.50, -.50, -.40, -.40, -.30,
      -.30, -.40, -.40, -.50, -.50, -.40, -.40, -.30,
      -.30, -.40, -.40, -.50, -.50, -.40, -.40, -.30,
      -.20, -.30, -.30, -.40, -.40, -.30, -.30, -.20,
      -.10, -.20, -.20, -.20, -.20, -.20, -.20, -.10,
        .20, .20,  0,  0,  0,  0, .20, .20,
        .20, .30, .10,  0,  0, .10, .30, .20
    )

    val kingValuesLate = Array(
       -.50, -.40, -.30, -.20, -.20, -.30, -.40, -.50,
       -.30, -.20, -.10,  0,  0, -.10, -.20, -.30,
       -.30, -.10, .20, .30, .30, .20, -.10, -.30,
       -.30, -.10, .30, .40, .40, .30, -.10, -.30,
       -.30, -.10, .30, .40, .40, .30, -.10, -.30,
       -.30, -.10, .20, .30, .30, .20, -.10, -.30,
       -.30, -.30,  0,  0,  0,  0, -.30, -.30,
       -.50, -.30, -.30, -.30, -.30, -.30, -.30, -.50
    )

    board.position.zipWithIndex
      .map { case (piece, index) =>
        piece.prop match {
          case "WN" => knightValues(index)
          case "WB" => bishopValues(index)
          case "WR" => rookValues(index)
          case "WK" => kingValuesEarly(index)
          case "WQ" => queenValues(index)
          case "BN" => -knightValues.reverse(index)
          case "BB" => -bishopValues.reverse(index)
          case "BR" => -rookValues.reverse(index)
          case "BK" => -kingValuesEarly.reverse(index)
          case "BQ" => -queenValues.reverse(index)
          case _    => 0
        }
      }
      .sum

  }

  def pawn = {
    def doublePawn = {
      val numWhiteDoublePawns = board.positionAsCols().count(li => li.count(piece => piece == Piece("WP")) > 1)
      val numBlackDoublePawns = board.positionAsCols().count(li => li.count(piece => piece == Piece("BP")) > 1)
      numBlackDoublePawns - numWhiteDoublePawns
    }

    def pawnIslands = {
      def numOfIslands(color: Char) = {
        var prevContainsPawn = false
        val cols = board.positionAsCols()
        val pawn = if (color == 'W') Piece("WP") else Piece("BP")
        (for (i <- 0 until 8) yield {
          if (cols(i).contains(pawn)){
            if (prevContainsPawn) 0
            else{
              prevContainsPawn = true
              1
            }
          }
          else {
            prevContainsPawn = false
            0
          }
        }).sum
      }
      numOfIslands('B') - numOfIslands('W')
    }

    0.4*doublePawn + 0.2*pawnIslands
  }

  def knight = {
    def decreasingWithFewerPawns = {
      ((numWhitePawns + numBlackPawns)/16)*
        (board.position.count(piece => piece.prop == "WN") - board.position.count(piece => piece.prop == "BN"))
    }
    0.2*decreasingWithFewerPawns
  }

  def bishop = {
    val whiteBishopPos = board.posFromPiece(Piece("WB"))
    val blackBishopPos = board.posFromPiece(Piece("BB"))

    def pair = {
      val whiteBishopPair = if (board.position.count(piece => piece.prop == "WB") == 2) 0.5 else 0
      val blackBishopPair = if (board.position.count(piece => piece.prop == "BB") == 2) 0.5 else 0
      whiteBishopPair - blackBishopPair
    }
    def fianchetto = {
      val whiteLeft   = if (board.position(49) == Piece("WB")) 1 else 0
      val whiteRight  = if (board.position(54) == Piece("WB")) 1 else 0
      val blackLeft   = if (board.position(9)  == Piece("WB")) 1 else 0
      val blackRight  = if (board.position(14) == Piece("WB")) 1 else 0
      whiteLeft + whiteRight - blackRight - blackRight
    }
    def badBishop = ???
    pair + 0.2*fianchetto
  }

  def rook = {
    val whiteRooks = board.posFromPiece(Piece("WR"))
    val blackRooks = board.posFromPiece(Piece("BR"))
    def increasingWithFewerPawns = {
      ((16 - numWhitePawns - numBlackPawns)/16)*
        (board.position.count(piece => piece.prop == "WR") - board.position.count(piece => piece.prop == "BR"))
    }
    def seventhRank = {
      (8 until 16).count(pos => board.position(pos).prop == "WR") -
        (48 until 56).count(pos => board.position(pos).prop == "BR")
    }

    // Advantage given to rooks behind passed pawns
    def tarraschRule = {
      val whitePawns = board.posFromPiece(Piece("WP"))
      val blackPawns = board.posFromPiece(Piece("BP"))
      val whitePawnsRows = whitePawns.map(pos => board.row(pos))
      val blackPawnsRows = blackPawns.map(pos => board.row(pos))

      val passedPawnsWhite = whitePawnsRows diff blackPawnsRows
      val passedPawnsBlack = blackPawnsRows diff whitePawnsRows

      whiteRooks.count{ pos => passedPawnsWhite.contains(board.row(pos)) } -
        blackRooks.count{ pos => passedPawnsBlack.contains(board.row(pos))}
    }

    //Give small bonus for having rook on same file as enemy queen
    def enemyQueenOnSameFile = {
      val whiteQueenFile = board.posFromPiece(Piece("WQ"))
      val blackQueenFile = board.posFromPiece(Piece("BQ"))

      whiteRooks.intersect(blackQueenFile).size - blackRooks.intersect(whiteQueenFile).size

    }

    0.3*increasingWithFewerPawns + 0.3*seventhRank + 0.2*tarraschRule + 0.1*enemyQueenOnSameFile
  }

  def mobility = {
    val moves = Moves(board)
    board.position
      .zipWithIndex
      .map { case (piece, pos) =>
        if (piece.color == 'W') moves.movesFromType(pos).size
        else if (piece.color == 'B') -moves.movesFromType(pos).size
        else 0
      }
      .sum * 0.01
  }
}
