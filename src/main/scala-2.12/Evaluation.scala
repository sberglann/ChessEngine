/**
  * Created by Sigurd on 13.01.2017.
  */
case class Evaluation(board: Board) {


  def totalHeuristics(): Double = {
    //material + pawn
    positionValues
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

  def knight() = {
    def decreasingWithFewerPawns = {
      (board.position.count(piece => piece.prop == "WP")/8)*board.position.count(piece => piece.prop == "WN") -
        (board.position.count(piece => piece.prop == "BP")/8)*board.position.count(piece => piece.prop == "BN")
    }
  }

  def bishop = ???

  def rook = ???
  
}
