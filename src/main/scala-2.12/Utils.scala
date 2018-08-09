object Utils {
  def row(i: Int): Int = i / 8
  def col(i: Int): Int = i % 8
  def index(i: Int, j: Int) = i * 8 + j
}
