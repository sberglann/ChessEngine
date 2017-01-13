
/**
  * Created by Sigurd on 03.01.2017.
  */
class Game {

  var board = Board(List(
    Piece("BR"), Piece("BN"), Piece("BB"), Piece("BQ"), Piece("BK"), Piece("BB"), Piece("BN"), Piece("BR"),
    Piece("BP"), Piece("BP"), Piece("BP"), Piece("BP"), Piece("BP"), Piece("BP"), Piece("BP"), Piece("BP"),
    Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"),
    Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"),
    Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"),
    Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"),
    Piece("WP"), Piece("WP"), Piece("WP"), Piece("WP"), Piece("WP"), Piece("WP"), Piece("WP"), Piece("WP"),
    Piece("WR"), Piece("WN"), Piece("WB"), Piece("WQ"), Piece("WK"), Piece("WB"), Piece("WN"), Piece("WR")),
    List(false, false, true, true, true, true), -1)
}

object Hei extends App {
  val g = new Game()
  println(g.board.prettyBoard)
  val moves = Moves(g.board)

  val e = Evaluation(g.board)
  println(e.totalHeuristics())
  val e2 = Evaluation(g.board.copy(position = g.board.position.updated(0, Piece("EE"))))
  println(e2.totalHeuristics())
}
