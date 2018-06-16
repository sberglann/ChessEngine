import Piece.Piece
import scalafx.application.{JFXApp, Platform}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.Includes._

object GUI extends JFXApp{

  var pieces: List[Piece] = List()
  val pieceProperty = ObjectProperty(pieces)

  stage = new PrimaryStage {
    title = "Chess"
    width = 1000
    height = 800
    scene = new Scene {
      val canvas = new BorderPane()

      val squares: GridPane  = createSquares()
      val pieces : GridPane  = createPieces(List())
      val stack  : StackPane = new StackPane()
      stack.children.add(squares)
      stack.children.add(pieces)

      val panel = createSidePanel()
      canvas.setCenter(stack)
      canvas.setLeft(panel)



      pieceProperty.onChange { (_, _, newPieces) =>
        canvas.children.removeIf { c =>
          Option(c.getId) match {
            case Some(id) if id.startsWith("pieces") => true
            case _ => false
          }
        }
        val pieces = createPieces(newPieces)
        stack.children.add(pieces)
      }



      content = canvas

    }
  }

  stage.sizeToScene()

  def updateBoard(board: Board): Unit = {
    /*val task = new javafx.concurrent.Task[Unit] {
      override def call(): Unit = {
        Platform.runLater(pieceProperty() = board.position)
      }
    }

    val t = new Thread(task, "update board")
    t.setDaemon(true)
    t.start()*/
    pieceProperty() = board.position
  }

  def createPieces(pieces: List[Piece]): GridPane = {
    val grid = new GridPane()
    grid.id = "pieces"

    val rc = (0 until 8).map(_ => new RowConstraints(100)).foreach(rc => grid.getRowConstraints.add(rc))
    val cc = (0 until 8).map(_ => new ColumnConstraints(100)).foreach(cc => grid.getColumnConstraints.add(cc))

    for {
      (p, i) <- pieces.zipWithIndex
      img <- toImg(p, i)
    } yield {
      val row = Utils.row(i)
      val col = Utils.col(i)
      grid.add(img, col, row)
    }

    grid
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
      imageView.id = s"piece_${index}_$piece"

      imageView
    }
  }

  private def createSquares(): GridPane = {
    val grid = new GridPane()
    grid.setHgap(0d)
    grid.setVgap(0d)

    // Add squares
    for {
      i <- 0 until 8
      j <- 0 until 8
      isWhite = i % 2 == j % 2
    } yield {
      val rect = new Rectangle()
      rect.setWidth(100)
      rect.setHeight(100)
      rect.fill = if (isWhite) Color.Wheat else Color.SaddleBrown
      grid.add(rect, i, j)
    }

    grid
  }

  private def createSidePanel(): VBox = {

    val vbox = new VBox()
    vbox.setMinHeight(800)
    vbox.setMinWidth(200)
    vbox.setBackground(new Background(Array(new BackgroundFill(Color.WhiteSmoke, null, null))))
    vbox.setPadding(Insets(20, 20, 20, 20))

    val startBtn = new Button()
    startBtn.setText("Start")
    startBtn.prefWidth = 200
    startBtn.onAction = handle {
      updateBoard(Board.defaultBoard)
    }

    vbox.children.add(startBtn)

    vbox
  }
}
