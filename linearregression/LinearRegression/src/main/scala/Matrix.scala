import Helpers.zipWith

import scala.math.sqrt
case class Matrix(m: Option[List[List[Double]]]) {

  type Tabular = List[List[Double]]

  def transpose: Matrix = m match {
    case None => Matrix()
    case Some(matrix) => Matrix(m.get.transpose)
  }


  def *(other: Matrix): Matrix = { // Validate dimensions
    if (m.head.length != other.m.head.length) Matrix()
    
    val data =
      for (line <- m.get) yield
      for (col <- other.transpose.m.get) yield
          (for (p <- line.zip(col)) yield
            p._1 * p._2).foldRight(0.0)(_ + _)


    Matrix(data)
  }





  def -(other: Matrix): Matrix = {
    (m, other.m) match {
      case (Some(matrix1), Some(matrix2)) if matrix1.length == matrix2.length && matrix1.head.length == matrix2.head.length =>
        val result = matrix1.zip(matrix2).map { case (row1, row2) =>
          row1.zip(row2).map { case (element1, element2) =>
            element1 - element2
          }
        }
        Matrix(Some(result))
      case _ => Matrix(None)
    }
  }

  def normalize: Matrix = ???

  def map(f: Double => Double): Matrix = ???

  def ++(x: Double): Matrix = ???

  def dimensions: String = ???

  // computes the mean squared error, only if this matrix is of dimension n x 1:
  def meanSquaredError: Option[Double] = ???

  override def toString: String = m.toString
}

object Matrix {

  def apply(): Matrix = Matrix(None)
  def apply(raw: List[List[Double]]): Matrix = Matrix(Some(raw))
  def apply(dataset: Dataset): Matrix = {
    val raw = dataset.data.tail.map(_.map(_.toDouble))
    Matrix(raw)
  }

  def apply(s: String): Matrix = {
    val raw = s.trim.split("\n").map { line =>
      line.trim.split("\\s+").map(_.toDouble).toList
    }.toList
    Matrix(raw)
  }

}
