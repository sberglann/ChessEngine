/**
  * Created by Sigurd on 03.01.2017.
  */

case class Piece (prop: String) {
  //Color represented as 'W', 'B' or 'E' (empty), piece types represented as 'K', 'Q', 'R', 'B', 'N', 'P' or 'E' (empty)
  lazy val color = prop.charAt(0)
  lazy val pieceType = prop.charAt(1)
  lazy val value = prop match {
    case "WK" => 100.0
    case "WQ" => 9.75
    case "WR" => 5.0
    case "WB" => 3.25
    case "WN" => 3.25
    case "WP" => 1.0
    case "BK" => -100.0
    case "BQ" => -9.75
    case "BR" => -5.0
    case "BB" => -3.25
    case "BN" => -3.25
    case "BP" => -1.0
    case _   => 0.0
  }

}

