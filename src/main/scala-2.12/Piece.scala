/**
  * Created by Sigurd on 03.01.2017.
  */

case class Piece (prop: String) {
  //Color represented as 'W', 'B' or 'E' (empty), piece types represented as 'K', 'Q', 'R', 'B', 'N', 'P' or 'E' (empty)
  lazy val color = prop.charAt(0)
  lazy val pieceType = prop.charAt(1)
  lazy val value = color match {
    case 'K' => 100.0
    case 'Q' => 9.75
    case 'R' => 5.0
    case 'B' => 3.25
    case 'N' => 3.25
    case 'P' => 1.0
    case _   => 0.0
  }

}

