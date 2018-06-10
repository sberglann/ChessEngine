import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
  * Created by Sigurd on 03.01.2017.
  */
class MovesTest extends FunSuite with BeforeAndAfterEach {


  test("Initial moves test") {
    val board = Board(List(
      Piece.br, Piece.bn, Piece.bb, Piece.bq, Piece.bk, Piece.bb, Piece.bn, Piece.br,
      Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp,
      Piece.wr, Piece.wn, Piece.wb, Piece.wq, Piece.wk, Piece.wb, Piece.wn, Piece.wr),
      List(false, false, true, true, true, true), -1)
    val moves = Moves(board)

    assert(moves.getMovesAt(0).toSet === Set())
    assert(moves.getMovesAt(1).toSet === Set(16, 18))
    assert(moves.getMovesAt(2).toSet === Set())
    assert(moves.getMovesAt(3).toSet === Set())
    assert(moves.getMovesAt(4).toSet === Set())
    assert(moves.getMovesAt(5).toSet === Set())
    assert(moves.getMovesAt(6).toSet === Set(21, 23))
    assert(moves.getMovesAt(7).toSet === Set())

    assert(moves.getMovesAt(8).toSet === Set(16, 24))
    assert(moves.getMovesAt(9).toSet === Set(17, 25))
    assert(moves.getMovesAt(10).toSet === Set(18, 26))
    assert(moves.getMovesAt(11).toSet === Set(19, 27))
    assert(moves.getMovesAt(12).toSet === Set(20, 28))
    assert(moves.getMovesAt(13).toSet === Set(21, 29))
    assert(moves.getMovesAt(14).toSet === Set(22, 30))
    assert(moves.getMovesAt(15).toSet === Set(23, 31))

    assert(moves.getMovesAt(48).toSet === Set(40, 32))
    assert(moves.getMovesAt(49).toSet === Set(41, 33))
    assert(moves.getMovesAt(50).toSet === Set(42, 34))
    assert(moves.getMovesAt(51).toSet === Set(43, 35))
    assert(moves.getMovesAt(52).toSet === Set(44, 36))
    assert(moves.getMovesAt(53).toSet === Set(45, 37))
    assert(moves.getMovesAt(54).toSet === Set(46, 38))
    assert(moves.getMovesAt(55).toSet === Set(47, 39))

    assert(moves.getMovesAt(56).toSet === Set())
    assert(moves.getMovesAt(57).toSet === Set(40, 42))
    assert(moves.getMovesAt(58).toSet === Set())
    assert(moves.getMovesAt(59).toSet === Set())
    assert(moves.getMovesAt(60).toSet === Set())
    assert(moves.getMovesAt(61).toSet === Set())
    assert(moves.getMovesAt(62).toSet === Set(45, 47))
    assert(moves.getMovesAt(63).toSet === Set())
  }

  test("Pawns removed initial moves test") {
    val board = Board(List(
      Piece.br, Piece.bn, Piece.bb, Piece.bq, Piece.bk, Piece.bb, Piece.bn, Piece.br,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.wr, Piece.wn, Piece.wb, Piece.wq, Piece.wk, Piece.wb, Piece.wn, Piece.wr),
      List(false, false, true, true, true, true), -1)

    val moves = Moves(board)
    assert(moves.getMovesAt(0).toSet === Set(8, 16, 24, 32, 40, 48, 56))
    assert(moves.getMovesAt(1).toSet === Set(11, 16, 18))
    assert(moves.getMovesAt(2).toSet === Set(9, 16, 11, 20, 29, 38, 47))
    assert(moves.getMovesAt(3).toSet === Set(10, 17, 24, 12, 21, 30, 39, 11, 19, 27, 35, 43, 51, 59))
    assert(moves.getMovesAt(4).toSet === Set(12, 13))
    assert(moves.getMovesAt(5).toSet === Set(12, 19, 26, 33, 40, 14, 23))
    assert(moves.getMovesAt(6).toSet === Set(12, 21, 23))
    assert(moves.getMovesAt(7).toSet === Set(15, 23, 31, 39, 47, 55, 63))

    assert(moves.getMovesAt(56).toSet === Set(48, 40, 32, 24, 16, 8, 0))
    assert(moves.getMovesAt(57).toSet === Set(40, 42, 51))
    assert(moves.getMovesAt(58).toSet === Set(49, 40, 51, 44, 37, 30, 23))
    assert(moves.getMovesAt(59).toSet === Set(50, 41, 32, 51, 43, 35, 27, 19, 11, 3, 52, 45, 38, 31))
    assert(moves.getMovesAt(60).toSet === Set(52, 53))
    assert(moves.getMovesAt(61).toSet === Set(52, 43, 34, 25, 16, 54, 47))
    assert(moves.getMovesAt(62).toSet === Set(45, 47, 52))
    assert(moves.getMovesAt(63).toSet === Set(55, 47, 39, 31, 23, 15, 7))
  }

  test("Pawn moves test") {
    val board = Board(List(
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.br, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.bp, Piece.ee, Piece.br, Piece.ee, Piece.ee, Piece.bb,
      Piece.ee, Piece.wq, Piece.ee, Piece.wp, Piece.bp, Piece.ee, Piece.wp, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.wk, Piece.ee
    ), List(false, false, false, false), -1)
    val moves = Moves(board)
    assert(moves.getMovesAt(51).toSet === Set(43, 35, 42, 44))
    assert(moves.getMovesAt(42).toSet === Set(49, 50, 51))
    assert(moves.getMovesAt(54).toSet === Set(46, 38))
  }

  test("En passant test") {
    val board = Board(List(
      Piece.br, Piece.bn, Piece.bb, Piece.bq, Piece.bk, Piece.bb, Piece.bn, Piece.br,
      Piece.bp, Piece.ee, Piece.bp, Piece.ee, Piece.bp, Piece.bp, Piece.bp, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.bp, Piece.wp, Piece.ee, Piece.wp, Piece.bp,
      Piece.wp, Piece.bp, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.ee, Piece.wp,
      Piece.wr, Piece.wn, Piece.wb, Piece.wq, Piece.wk, Piece.wb, Piece.wn, Piece.wr),
      List(false, false, true, true, true, true), 3)
    val moves = Moves(board)
    assert(moves.getMovesAt(28).toSet === Set(19, 20))
    assert(moves.getMovesAt(30).toSet === Set(22))

    val newCol = moves.copy(board = board.copy(enPassantCol = 0))
    assert(newCol.getMovesAt(33).toSet === Set(41, 40))
  }

  test("Bishop moves test") {
    val board = Board(List(
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.br, Piece.ee,
      Piece.bk, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.bb, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.wr, Piece.ee, Piece.ee, Piece.wb, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.bp, Piece.ee, Piece.br, Piece.ee, Piece.ee, Piece.bb,
      Piece.ee, Piece.wq, Piece.ee, Piece.wp, Piece.bp, Piece.wp, Piece.wp, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.wb, Piece.ee, Piece.wk, Piece.ee
    ), List(false, false, false, false), -1)
    val moves = Moves(board)
    assert(moves.getMovesAt(47).toSet === Set(54, 38, 29, 20, 11, 2))
    assert(moves.getMovesAt(27).toSet === Set(18, 9, 0, 20, 13, 6, 34, 41, 48, 36, 45))
    assert(moves.getMovesAt(60).toSet === Set())
    assert(moves.getMovesAt(16).toSet === Set())
  }

  test("Rook moves test") {
    val board = Board(List(
      Piece.ee, Piece.ee, Piece.ee, Piece.bk, Piece.ee, Piece.ee, Piece.br, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.br, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.bp, Piece.wr, Piece.br, Piece.ee, Piece.ee, Piece.bb,
      Piece.ee, Piece.wq, Piece.ee, Piece.wp, Piece.bp, Piece.ee, Piece.wp, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.wk, Piece.ee
    ), List(false, false, false, false), -1)
    val moves = Moves(board)
    assert(moves.getMovesAt(27).toSet === Set(19, 11, 35, 43))
    assert(moves.getMovesAt(43).toSet === Set(35, 27, 42, 44))
    assert(moves.getMovesAt(44).toSet === Set(36, 28, 20, 12, 4, 45, 46, 43))
  }

  test("Knight moves test") {
    val board = Board(List(
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.br, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.wk, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.br, Piece.ee, Piece.wn, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.bb, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.wn, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.bp, Piece.ee, Piece.br, Piece.ee, Piece.ee, Piece.bn,
      Piece.ee, Piece.wq, Piece.ee, Piece.wp, Piece.bp, Piece.ee, Piece.wp, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee
    ), List(false, false, false, false), -1)
    val moves = Moves(board)
    assert(moves.getMovesAt(18).toSet === Set())
    assert(moves.getMovesAt(33).toSet === Set(48, 50, 43, 27, 16))
  }

  test("Castle west white") {
    val board = Board(List(
      Piece.br, Piece.bn, Piece.bb, Piece.bq, Piece.bk, Piece.bb, Piece.bn, Piece.br,
      Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp,
      Piece.wr, Piece.wn, Piece.ee, Piece.ee, Piece.wk, Piece.wb, Piece.wn, Piece.wr),
      List(false, false, true, true, true, true), -1)

    val blockingPiece = Moves(board)
    assert(blockingPiece.castleWestWhite === List())

    val clear = blockingPiece.copy(board = board.deletePiece(57))
    assert(clear.castleWestWhite != List())

    val pathChecked = clear.copy(board = clear.board.deletePiece(51).changedBoard(3, 19))
    assert(pathChecked.castleWestWhite === List())

    val kingChecked = clear.copy(board = pathChecked.board.deletePiece(52).changedBoard(19, 20))
    assert(pathChecked.castleWestWhite === List())

    val clear2 = pathChecked.copy(board = pathChecked.board.deletePiece(19))
    assert(clear2.castleWestWhite != List())

    val board2 = board.copy(position = board.position.updated(57, Piece.ee), info = List(false, false, false, true, true, true))
    val illegalMoveHistory = Moves(board2)
    assert(illegalMoveHistory.castleWestWhite === List())
  }

  test("Castle east black") {
    val board = Board(List(
      Piece.br, Piece.bn, Piece.bb, Piece.bq, Piece.bk, Piece.ee, Piece.bn, Piece.br,
      Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp,
      Piece.wr, Piece.wn, Piece.wb, Piece.wq, Piece.wk, Piece.wb, Piece.wn, Piece.wr),
      List(false, false, true, true, true, true), -1)

    val blockingPiece = Moves(board)
    assert(blockingPiece.castleEastBlack === List())

    val clear = blockingPiece.copy(board = board.deletePiece(6))
    assert(clear.castleEastBlack != List())

    val pathChecked = clear.copy(board = clear.board.deletePiece(13).changedBoard(59, 45))
    assert(pathChecked.castleEastBlack === List())

    val kingChecked = clear.copy(board = pathChecked.board.deletePiece(12).changedBoard(45, 44))
    assert(pathChecked.castleEastBlack === List())

    val clear2 = pathChecked.copy(board = pathChecked.board.deletePiece(45))
    assert(clear2.castleEastBlack != List())

    val board2 = board.copy(position = board.position.updated(6, Piece.ee), info = List(false, false, true, true, true, false))
    val illegalMoveHistory = Moves(board2)
    assert(illegalMoveHistory.castleEastBlack === List())
  }

  test("Castle east white") {
    val board = Board(List(
      Piece.br, Piece.bn, Piece.bb, Piece.bq, Piece.bk, Piece.bb, Piece.bn, Piece.br,
      Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp,
      Piece.wr, Piece.wn, Piece.ee, Piece.ee, Piece.wk, Piece.ee, Piece.wn, Piece.wr),
      List(false, false, true, true, true, true), -1)

    val blockingPiece = Moves(board)
    assert(blockingPiece.castleEastWhite === List())

    val clear = blockingPiece.copy(board = board.deletePiece(62))
    assert(clear.castleEastWhite != List())

    val pathChecked = clear.copy(board = clear.board.deletePiece(53).changedBoard(3, 21))
    assert(pathChecked.castleEastWhite === List())

    val kingChecked = clear.copy(board = pathChecked.board.deletePiece(52).changedBoard(21, 20))
    assert(pathChecked.castleEastWhite === List())

    val clear2 = pathChecked.copy(board = pathChecked.board.deletePiece(21))
    assert(clear2.castleEastWhite != List())

    val board2 = board.copy(position = board.position.updated(62, Piece.ee), info = List(false, false, true, true, false, true))
    val illegalMoveHistory = Moves(board2)
    assert(illegalMoveHistory.castleEastWhite === List())
  }

  test("Castle west black") {
    val board = Board(List(
      Piece.br, Piece.bn, Piece.ee, Piece.ee, Piece.bk, Piece.bb, Piece.bn, Piece.br,
      Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp, Piece.bp,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee, Piece.ee,
      Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp, Piece.wp,
      Piece.wr, Piece.wn, Piece.wb, Piece.wq, Piece.wk, Piece.wb, Piece.wn, Piece.wr),
      List(false, false, true, true, true, true), -1)
    
    val blockingPiece = Moves(board)
    assert(blockingPiece.castleWestBlack === List())

    val clear = blockingPiece.copy(board = board.deletePiece(1))
    assert(clear.castleWestBlack != List())

    val pathChecked = clear.copy(board = clear.board.deletePiece(11).changedBoard(59, 43))
    assert(pathChecked.castleWestBlack === List())

    val kingChecked = clear.copy(board = pathChecked.board.deletePiece(12).changedBoard(43, 44))
    assert(pathChecked.castleWestBlack === List())

    val clear2 = pathChecked.copy(board = pathChecked.board.deletePiece(43))
    assert(clear2.castleWestBlack != List())

    val board2 = board.copy(position = board.position.updated(6, Piece.ee), info = List(false, false, true, false, true, true))
    val illegalMoveHistory = Moves(board2)
    assert(illegalMoveHistory.castleWestBlack === List())
  }
}
