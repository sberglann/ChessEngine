object Piece extends Enumeration {

  type Piece = Value
  implicit def toVal(x: Value): Val = x.asInstanceOf[Val]
  //Color represented as 'W', 'B' or 'E' (empty), piece types represented as 'K', 'Q', 'R', 'B', 'N', 'P' or 'E' (empty)
  case class Val(color: Char, pieceType: Char, value: Double) extends super.Val {
    override def toString(): String = s"$color$pieceType"
  }

  val wk = Val('W', 'K', 100)
  val wq = Val('W', 'Q', 9.75)
  val wr = Val('W', 'R', 5.0)
  val wb = Val('W', 'B', 3.25)
  val wn = Val('W', 'N', 3.25)
  val wp = Val('W', 'P', 1.0)
  val bk = Val('B', 'K', -100)
  val bq = Val('B', 'Q', -9.75)
  val br = Val('B', 'R', -5.0)
  val bb = Val('B', 'B', -3.25)
  val bn = Val('B', 'N', -3.25)
  val bp = Val('B', 'P', -1.0)
  val ee = Val('E', 'E', 0)
}
