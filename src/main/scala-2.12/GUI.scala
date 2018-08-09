import Piece.Piece

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

object GUI extends JFXApp{

  var board: Board = null
  val boardProperty = ObjectProperty(board)
  var highlighted: List[javafx.scene.Node] = List()
  var selected: Option[ImageView] = None
  var legalMoves: List[Int] = List()
  var whitesTurn = true

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



      boardProperty.onChange { (_, _, newBoard) =>
        stack.children.removeIf { c =>
          Option(c.getId) match {
            case Some(id) if id.startsWith("pieces") => true
            case _ => false
          }
        }
        val pieces = createPieces(newBoard.position)
        stack.children.add(pieces)
      }



      content = canvas

    }
  }

  stage.sizeToScene()

  def updateBoard(board: Board): Unit = {
    boardProperty() = board
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
    grid.setPickOnBounds(false)

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
      imageView.id = s"piece_${piece}"

      imageView.handleEvent(MouseEvent.MousePressed) { evt: MouseEvent => initMovePiece(imageView, Utils.row(index), Utils.col(index)) }
      imageView
    }
  }

  def initMovePiece(image: ImageView, i: Int, j: Int): Unit = {
    if (isWhite(image) == whitesTurn) {
      deHighligt()

      legalMoves = Moves(boardProperty.value).getMovesAt(Utils.index(i, j))

      val gridMoves = for {
        c <- squares().children if legalMoves.contains(Utils.index(GridPane.getRowIndex(c), GridPane.getColumnIndex(c)))
      } yield {
        c
      }
      gridMoves.foreach(n => n.setOpacity(0.4))
      highlighted = gridMoves.toList

      selected = Some(image)
      image.setOpacity(0.4)
    }
  }

  def movePiece(rect: Rectangle, i: Int, j: Int): Unit = {
    val newIndex = Utils.index(i, j)
    selected match {
      case Some(pieceImg) if legalMoves.contains(newIndex) =>
        val oldRow = GridPane.getRowIndex(pieceImg)
        val oldCol = GridPane.getColumnIndex(pieceImg)
        val oldIndex = Utils.index(oldRow, oldCol)

        val piece = boardProperty.value.position(oldIndex)

        whitesTurn = !whitesTurn
        val newBoard = boardProperty.value.changedBoard(oldIndex, newIndex)
        updateBoard(newBoard)
      case _ =>
        deHighligt()
    }
  }

  private def deHighligt(): Unit = {
    highlighted.foreach(_.setOpacity(1.0))
    highlighted = List()
    selected.foreach(_.setOpacity(1.0))
    selected = None
  }
  
  private def createSquares(): GridPane = {
    val grid = new GridPane()
    grid.setHgap(0d)
    grid.setVgap(0d)
    grid.id = "squares"

    // Add squares
    for {
      j <- 0 until 8
      i <- 0 until 8
      isWhite = i % 2 == j % 2
    } yield {
      val rect = new Rectangle()
      rect.setWidth(100)
      rect.setHeight(100)
      rect.fill = if (isWhite) Color.Wheat else Color.SaddleBrown

      rect.handleEvent(MouseEvent.MouseReleased) { evt: MouseEvent => movePiece(rect, j, i) }

      grid.add(rect, i, j)
    }

    grid
  }

  private def squares(): GridPane = {
    stage.scene.value
      .getChildren.head.asInstanceOf[javafx.scene.layout.BorderPane]
      .getCenter.asInstanceOf[javafx.scene.layout.StackPane]
      .getChildren.find(_.id.value == "squares").get.asInstanceOf[javafx.scene.layout.GridPane]
  }

  private def pieces(): GridPane = {
    stage.scene.value
      .getChildren.head.asInstanceOf[javafx.scene.layout.BorderPane]
      .getCenter.asInstanceOf[javafx.scene.layout.StackPane]
      .getChildren.find(_.id.value == "pieces").get.asInstanceOf[javafx.scene.layout.GridPane]
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

  private def isWhite(img: ImageView): Boolean = {
    img.id.value.startsWith("piece_W")
  }
}
