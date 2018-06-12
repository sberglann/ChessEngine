import Piece.Piece

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

/**
  * Created by Sigurd on 22.01.2017.
  */

object GUI extends JFXApp{

  var pieces: List[Piece.Piece] = Game.board.position
  val pieceProperty = ObjectProperty(pieces)

  stage = new PrimaryStage {
    title = "Chess"
    width = 800
    height = 800
    scene = new Scene {
      val canvas = new Pane()
      val squares = for {
        i <- 0 until 8
        j <- 0 until 8
        isWhite = i % 2 == j % 2
      } yield {
        val rect = new Rectangle()
        rect.setWidth(100)
        rect.setHeight(100)
        rect.setX(i*100)
        rect.setY(j*100)
        rect.fill = if (isWhite) Color.White else Color.Black
        canvas.children.add(rect)
      }


      pieceProperty.onChange { (_, _, newPieces) =>
        canvas.children.removeIf { c =>
          Option(c.getId) match {
            case Some(id) if id.startsWith("piece") => true
            case _ => false
          }

          val imgs = newPieces.zipWithIndex.flatMap{ case (p, i) => toImg(p, i)}
          imgs.forall(canvas.children.add(_))
        }
      }



      content = canvas

    }
  }

  stage.sizeToScene()

  def updateBoard(board: Board): Unit = {
    pieceProperty() = board.position
  }

  def toImg(piece: Piece, index: Int): Option[ImageView] = {

    val image = piece match {
      case Piece.wk => Some(new Image("img/wk.png"))
      case Piece.wq => Some(new Image("img/wq.png"))
      case Piece.wr => Some(new Image("img/wr.png"))
      case Piece.wb => Some(new Image("img/wb.png"))
      case Piece.wn => Some(new Image("img/wn.png"))
      case Piece.wp => Some(new Image("img/wp.png"))
      case Piece.bk => Some(new Image("img/bk.png"))
      case Piece.bq => Some(new Image("img/bq.png"))
      case Piece.br => Some(new Image("img/br.png"))
      case Piece.bb => Some(new Image("img/bb.png"))
      case Piece.bn => Some(new Image("img/bn.png"))
      case Piece.bp => Some(new Image("img/bp.png"))
      case _ => None
    }

    image.map{ img =>
      val imageView = new ImageView()
      imageView.image = img
      imageView.fitHeight = 100
      imageView.fitWidth = 100
      imageView.x = Utils.col(index) * 100
      imageView.y = Utils.row(index) * 100
      imageView.id = s"piece_${index}_$piece"

      imageView
    }
  }
}
