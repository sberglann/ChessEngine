
/**
  * Created by Sigurd on 03.01.2017.
  */
class Game {

  var board = Board(List(
    Piece("BR"), Piece("BN"), Piece("BB"), Piece("BQ"), Piece("BK"), Piece("BB"), Piece("BN"), Piece("BR"),
    Piece("BP"), Piece("EE"), Piece("BP"), Piece("BP"), Piece("BP"), Piece("BP"), Piece("BP"), Piece("BP"),
    Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"),
    Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"),
    Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"),
    Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"), Piece("EE"),
    Piece("WP"), Piece("WP"), Piece("WP"), Piece("WP"), Piece("WP"), Piece("WP"), Piece("WP"), Piece("WP"),
    Piece("WR"), Piece("WN"), Piece("WB"), Piece("WQ"), Piece("WK"), Piece("WB"), Piece("WN"), Piece("WR")),
    List(false, false, false ,false))
}

object Hei extends App {
  val g = new Game()
  println(g.board.prettyBoard)
  val moves = Moves(g.board)
}
