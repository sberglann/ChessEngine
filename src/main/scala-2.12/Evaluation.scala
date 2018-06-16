case class Evaluation(board: Board) {

  val numWhitePawns = board.position.count(_ == Piece.wp)
  val numBlackPawns = board.position.count(_ == Piece.bp)

  def totalHeuristics(): Double = {
    //material + pawn
    positionValues + knight + bishop + rook + mobility
  }

  def quickEval(color: Char) = if (color == 'W') material else -material

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
        piece match {
          case Piece.wn => knightValues(index)
          case Piece.wb => bishopValues(index)
          case Piece.wr => rookValues(index)
          case Piece.wk => kingValuesEarly(index)
          case Piece.wq => queenValues(index)
          case Piece.bn => -knightValues.reverse(index)
          case Piece.bb => -bishopValues.reverse(index)
          case Piece.br => -rookValues.reverse(index)
          case Piece.bk => -kingValuesEarly.reverse(index)
          case Piece.bq => -queenValues.reverse(index)
          case _    => 0
        }
      }
      .sum

  }

  def pawn = {
    def doublePawn = {
      val numWhiteDoublePawns = board.positionAsCols().count(li => li.count(piece => piece == Piece.wp) > 1)
      val numBlackDoublePawns = board.positionAsCols().count(li => li.count(piece => piece == Piece.bp) > 1)
      numBlackDoublePawns - numWhiteDoublePawns
    }

    def pawnIslands = {
      def numOfIslands(color: Char) = {
        var prevContainsPawn = false
        val cols = board.positionAsCols()
        val pawn = if (color == 'W') Piece.wp else Piece.bp
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

  def knight: Double = {
    def decreasingWithFewerPawns = {
      ((numWhitePawns + numBlackPawns)/16)*
        (board.position.count(_ == Piece.wn) - board.position.count(_ == Piece.bn))
    }
    0.2*decreasingWithFewerPawns
  }

  def bishop: Double = {
    val whiteBishopPos = board.posFromPiece(Piece.wb)
    val blackBishopPos = board.posFromPiece(Piece.bb)

    def pair = {

      val whiteBishopPair = if (board.position.count(_ == Piece.wb) == 2) 0.5 else 0
      val blackBishopPair = if (board.position.count(_ == Piece.bb) == 2) 0.5 else 0
      whiteBishopPair - blackBishopPair
    }
    def fianchetto = {
      val whiteLeft   = if (board.position(49) == Piece.wb) 1 else 0
      val whiteRight  = if (board.position(54) == Piece.wb) 1 else 0
      val blackLeft   = if (board.position(9)  == Piece.wb) 1 else 0
      val blackRight  = if (board.position(14) == Piece.wb) 1 else 0
      whiteLeft + whiteRight - blackRight - blackRight
    }
    def badBishop = ???
    pair + 0.2*fianchetto
  }

  def rook: Double = {
    val whiteRooks = board.posFromPiece(Piece.wr)
    val blackRooks = board.posFromPiece(Piece.br)
    def increasingWithFewerPawns = {
      ((16 - numWhitePawns - numBlackPawns)/16)*
        (board.position.count(_== Piece.wr) - board.position.count(_ == Piece.br))
    }
    def seventhRank = {
      (8 until 16).count(pos => board.position(pos) == Piece.wr) -
        (48 until 56).count(pos => board.position(pos) == Piece.br)
    }

    // Advantage given to rooks behind passed pawns
    def tarraschRule = {
      val whitePawns = board.posFromPiece(Piece.wp)
      val blackPawns = board.posFromPiece(Piece.bp)
      val whitePawnsRows = whitePawns.map(pos => board.row(pos))
      val blackPawnsRows = blackPawns.map(pos => board.row(pos))

      val passedPawnsWhite = whitePawnsRows diff blackPawnsRows
      val passedPawnsBlack = blackPawnsRows diff whitePawnsRows

      whiteRooks.count{ pos => passedPawnsWhite.contains(board.row(pos)) } -
        blackRooks.count{ pos => passedPawnsBlack.contains(board.row(pos))}
    }

    //Give small bonus for having rook on same file as enemy queen
    def enemyQueenOnSameFile = {
      val whiteQueenFile = board.posFromPiece(Piece.wq)
      val blackQueenFile = board.posFromPiece(Piece.bq)

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
