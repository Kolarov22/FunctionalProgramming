import scala.math.sqrt

object Regression extends App{
  def profileID = 2

  def dataset = Dataset("datasets/houseds.csv")
  def steps = 10000      // the number of steps for gradient descent
  def alpha = 0.000000000001

  // X: n x 2  prediction: 2 x 1
  def rootMeanSquareError(X: Matrix, Y: Matrix, parameters: Matrix): Option[Double] = ???

  // recursion step for the gradient
  def gradientDescentStep(X: Matrix, Y: Matrix, parameters: Matrix): Matrix = ???


  def linearRegression(steps: Int, parameters: Matrix, features: List[String]): (Matrix,Option[Double]) = ???

  /* test your linear regression here, compute your prediction for (at least) two points, and plot them together
     with the dataset, as specified in the statement
   */


}
