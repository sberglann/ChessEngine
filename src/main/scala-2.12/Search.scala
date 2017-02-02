import scala.annotation.tailrec

/**
  * Created by Sigurd on 24.01.2017.
  */
case class Search(maxColor: Char, initialBoard: Board) {

  val maxDepth = 2
  val minColor = if (maxColor == 'W') 'B' else 'W'

  def runSearch: (Double, Board) = {
    val successors = initialBoard.generateSuccessor(maxColor)
    if (successors.isEmpty) (terminalEvaluator(initialBoard, true), initialBoard)
    else maximize(successors, 0, -1000, 1000, -1000)
  }

  @tailrec
  private def maximize(children: List[Board], depth: Int, alpha: Double, beta: Double, value: Double): (Double, Board) = {
    val node = children.head
    if (depth >= maxDepth) return (eval(node), node)
    val successors = node.generateSuccessor(minColor).sortBy(board => -Evaluation(board).quickEval(minColor))

    val newValue =
      if (successors.nonEmpty) math.max(value, minimize(successors, depth + 1, alpha, beta, -1000)._1)
      else return (terminalEvaluator(node, false), node)
    val newAlpha = math.max(alpha, newValue)

    if (beta <= newAlpha || children.tail.isEmpty) (newValue, children.head)
    else maximize(children.tail, depth, newAlpha, beta, newValue)
  }

  @tailrec
  private def minimize(children: List[Board], depth: Int, alpha: Double, beta: Double, value: Double): (Double, Board) = {
    val node = children.head
    if (depth >= maxDepth) return (eval(node), node)
    val successors = node.generateSuccessor(maxColor).sortBy(board => Evaluation(board).quickEval(maxColor))

    val newValue =
      if (successors.nonEmpty) math.min(value, maximize(successors, depth + 1, alpha, beta, 1000)._1)
      else return (terminalEvaluator(node, true), node)

    val newBeta = math.min(beta, newValue)

    if (newBeta <= alpha || children.tail.isEmpty) (newValue, children.head)
    else minimize(children.tail, depth, alpha, newBeta, newValue)
  }


  //Black's score has to be negated when searching from black
  def eval(node: Board): Double = {
    if (maxColor == 'W') Evaluation(node).totalHeuristics else -Evaluation(node).totalHeuristics
  }

  def checkmate(node: Board): Boolean = {
    if (node.position.contains(Piece("WK")) && node.position.contains(Piece("BK"))) false
    else true
  }

  def terminalEvaluator(node: Board, maxPlayer: Boolean): Double = {
    val moves = Moves(node)
    if (moves.check(maxColor, node)) -1000
    else if (moves.check(minColor, node)) 1000
    else 0
  }


}
