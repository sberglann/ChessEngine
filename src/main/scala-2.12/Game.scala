
/**
  * Created by Sigurd on 03.01.2017.
  */
object Game {

  val board = Board(List(
    Piece.br, Piece.bn, Piece.bb, Piece.bq, Piece.bk, Piece.bb, Piece.bn, Piece.br,
    Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp,
    Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
    Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
    Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
    Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.wp, Piece.ee, Piece.ee, Piece.ee,
    Piece.wp, Piece.wp, Piece.wp, Piece.ee, Piece.wp, Piece.wp, Piece.wp, Piece.wp,
    Piece.wr, Piece.wn, Piece.wb, Piece.wq, Piece.wk, Piece.wb, Piece.wn, Piece.wr
  ), List(false, false, true, true, true, true), -1)

  val board3 = Board(List(
    Piece.br, Piece.bn, Piece.bb, Piece.ee, Piece.ee, Piece.br, Piece.bk, Piece.ee,
    Piece.bp, Piece.bp, Piece.ee, Piece.ee, Piece.bp, Piece.bp, Piece.ee, Piece.bp,
    Piece.ee, Piece.ee, Piece.bp, Piece.bq, Piece.ee, Piece.bn, Piece.bp, Piece.bb,
    Piece.ee, Piece.ee, Piece.ee, Piece.bp, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
    Piece.ee, Piece.ee, Piece.ee, Piece.wp, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
    Piece.wn, Piece.ee, Piece.ee, Piece.ee, Piece.wp, Piece.wn, Piece.ee, Piece.ee,
    Piece.wp, Piece.wp, Piece.wp, Piece.ee, Piece.wk, Piece.wp, Piece.wp, Piece.wp,
    Piece.ee, Piece.br, Piece.wb, Piece.wq, Piece.ee, Piece.wb, Piece.wr, Piece.ee
  ), List(false, false, true, true, true, true), -1)

  val board2 = Board(List(
    Piece.bk, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
    Piece.ee, Piece.ee, Piece.wp, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
    Piece.ee, Piece.ee, Piece.wk, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
    Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
    Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
    Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
    Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
    Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee
  ), List(false, false, false, false, false, false), -1)
}

object Hei extends App {

  val gui = GUI
  gui.updateBoard(Game.board)

  println(Game.board3.prettyBoard)



  //val succ = g.board3.generateSuccessor('W').sortBy(board => Evaluation(board).totalHeuristics)
  //succ.foreach(b => println(b.prettyBoard))

  val now = System.currentTimeMillis
  val s1 = Search('W', Game.board3)
  println(s1.runSearch._2.prettyBoard)

  val s2 = Search('B', s1.runSearch._2)
  println(s2.runSearch._2.prettyBoard)

  val s3 = Search('W', s2.runSearch._2)
  println(s3.runSearch._2.prettyBoard)

  val s4 = Search('B', s3.runSearch._2)
  println(s4.runSearch._2.prettyBoard)

  val s5 = Search('W', s4.runSearch._2)
  println(s5.runSearch._2.prettyBoard)

  val s6 = Search('B', s5.runSearch._2)
  println(s6.runSearch._2.prettyBoard)

  val s7 = Search('W', s6.runSearch._2)
  println(s7.runSearch._2.prettyBoard)

  val s8 = Search('B', s7.runSearch._2)
  println(s8.runSearch._2.prettyBoard)

  val s9 = Search('W', s8.runSearch._2)
  println(s9.runSearch._2.prettyBoard)

  val deltaSec = (System.currentTimeMillis-now)/1000.0
  println(s"Time elapsed: $deltaSec seconds")

}
