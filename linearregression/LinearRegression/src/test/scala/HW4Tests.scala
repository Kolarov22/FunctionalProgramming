import Regression._

class HW4Tests extends munit.FunSuite {
  val test_ds = Dataset("datasets/tinyds.csv")
  // Tolerance for result and sum of errors
  val tolerance: Double = 0.05
  test("Valid profile id: "+ profileID){
        assert(profileID > 0)
      }
  //Dataset Tests
  test("Select columns (20p):"){
    val selcol = test_ds.selectColumns(List("GrLivArea"))
    val selcols = test_ds.selectColumns(List("GrLivArea","GarageArea"))
    assert(selcol.getRows == List(List("100"), List("200"), List("210"), List("300")))
    assert(selcol.getColumns == List("GrLivArea"))
    assert(selcols.getRows == List(List("100", "548"), List("200", "460"), List("210", "608"), List("300", "642")))
    assert(selcols.getColumns == List("GrLivArea", "GarageArea"))
  }

  test("Split 1 (3p):") {

    println(test_ds.split(0.0))
    println(test_ds.split(0.0)._1)
    println(test_ds.split(0.0)._2)
    assert(test_ds.split(0.0)._1 == Dataset(List(List("GrLivArea", "GarageArea", "SalePrice"), List("100", "548", "50000"), List("210", "608", "100500"), List("300", "642", "153000"))))
    assert(test_ds.split(0.0)._2 == Dataset(List(List("GrLivArea", "GarageArea", "SalePrice"))))
  }

  test("Split 2 (3p):") {

    assert(test_ds.split(0.5)._1 == Dataset(List(List("GrLivArea", "GarageArea", "SalePrice"), List("100", "548", "50000"), List("300", "642", "153000"))))
    assert(test_ds.split(0.5)._2 == Dataset(List(List("GrLivArea", "GarageArea", "SalePrice"), List("200", "460", "100000"), List("210", "608", "100500"))))
  }

  test("Split 3 (4p):") {
    assert(test_ds.split(1.0)._1 == Dataset(List(List("GrLivArea", "GarageArea", "SalePrice"))))
    assert(test_ds.split(1.0)._2 == Dataset(List(List("GrLivArea", "GarageArea", "SalePrice"), List("200", "460", "100000"), List("100", "548", "50000"), List("210", "608", "100500"), List("300", "642", "153000"))))
  }

  //Matrix Tests
  test("Transpose (10p):"){
    //Square matrix
    assert(
      Matrix(List(List(1.0, 2.0, 3.0),
        List(4.0, 5.0, 6.0),
        List(7.0, 8.0, 9.0))).transpose ==
        Matrix(List(List(1.0, 4.0, 7.0),
          List(2.0, 5.0, 8.0),
          List(3.0, 6.0, 9.0)))
    )
    //Rectangular matrix
    assert(
      Matrix(List(List(1.0, 2.0, 3.0),
        List(4.0, 5.0, 6.0))).transpose ==
        Matrix(List(List(1.0, 4.0),
          List(2.0, 5.0),
          List(3.0, 6.0)))
    )
    //Empty
    assert(Matrix(List()).transpose == Matrix(List()))
    //Single row
    assert(Matrix(List(List(1.0, 2.0, 3.0))).transpose == Matrix(List(List(1.0), List(2.0), List(3.0))))
    //Single Column
    assert(Matrix(List(List(1.0), List(2.0), List(3.0))).transpose == Matrix(List(List(1.0, 2.0, 3.0))))
  }
  test("Multiplication test 1 (2p):") {

    val ds1 =
      """1.0 2.0
        |3.0 4.0""".stripMargin
    val ds2 =
      """2.0 0.0
        |1.0 2.0""".stripMargin

    val ds3 =
      """4.0 4.0
        |10.0 8.0""".stripMargin

    assert(Matrix(ds1) * Matrix(ds2) == Matrix(ds3))
  }
  test("Multiplication test 2 (2p):") {
    val ds4 =
      """1.0 2.0 3.0
        |4.0 5.0 6.0
        |7.0 8.0 9.0""".stripMargin

    val ds5 =
      """7.0 8.0
        |9.0 10.0
        |11.0 12.0""".stripMargin

    val ds6 =
      """58.0 64.0
        |139.0 154.0
        |220.0 244.0""".stripMargin

    assert(Matrix(ds4) * Matrix(ds5) == Matrix(ds6))
  }
  test("Multiplication test 3 (2p):") {
    val ds7 =
      """1.0 2.0 3.0""".stripMargin

    val ds8 =
      """4.0
        |5.0
        |6.0""".stripMargin

    assert(Matrix(ds7) * Matrix(ds8) == Matrix(List(List(32.0))))
  }
  test("Multiplication test 4 (2p):") {
    val ds9 =
      """4.0
        |5.0
        |6.0""".stripMargin

    val ds10 =
      """1.0 2.0 3.0""".stripMargin

    val ds11 =
      """4.0 8.0 12.0
        |5.0 10.0 15.0
        |6.0 12.0 18.0""".stripMargin

    //Single column * Single row ?
    assert(Matrix(ds9) * Matrix(ds10) == Matrix(ds11))
  }
  test("Multiplication test 5 (2p):") {
    val ds12 =
      """1.0 2.0 3.0
        |4.0 5.0 6.0""".stripMargin

    val ds13 =
      """7.0 8.0
        |9.0 10.0""".stripMargin
    //None
    assert(Matrix(ds12) * Matrix(ds13) == Matrix())
  }


  test("Minus test 1 (2p):") {
    val ds1 =
      """4.0 5.0
        |6.0 7.0""".stripMargin

    val ds2 =
      """1.0 2.0
        |3.0 4.0""".stripMargin

    val ds3 =
      """3.0 3.0
        |3.0 3.0""".stripMargin

    assert(Matrix(ds1) - Matrix(ds2) == Matrix(ds3))
  }
  test("Minus test 2 (2p):") {

      val ds1 =
        """4.0 5.0 6.0
          |7.0 8.0 9.0""".stripMargin

      val ds2 =
        """1.0 2.0 3.0
          |4.0 5.0 6.0""".stripMargin

      val ds3 =
        """3.0 3.0 3.0
          |3.0 3.0 3.0""".stripMargin
      assert(Matrix(ds1) - Matrix(ds2) == Matrix(ds3))
    }
  test("Minus test 3 (3p):") {

    val ds1 =
      """4.0 5.0 6.0""".stripMargin

    val ds2 =
      """1.0 2.0 3.0""".stripMargin

    val ds3 =
      """3.0 3.0 3.0""".stripMargin

    assert(Matrix(ds1) - Matrix(ds2) == Matrix(ds3))

  }
  test("Minus test 4 (3p):") {

    val ds1 =
      """4.0 5.0 6.0
        |7.0 8.0 9.0""".stripMargin

    val ds2 =
      """1.0 2.0 3.0""".stripMargin
    //None
    assert(Matrix(ds1) - Matrix(ds2) == Matrix())
  }

  test("Map (10p):") {
    val matrix = Matrix(List(List(1.0, 2.0, 3.0), List(4.0, 5.0, 6.0)))
    val mappedMatrix = matrix.map(_ * 2)
    assert(mappedMatrix == Matrix(List(List(2.0, 4.0, 6.0), List(8.0, 10.0, 12.0))))
  }

  test("Prepend Column (0p):") {
    val matrix = Matrix(List(List(1.0, 2.0), List(3.0, 4.0)))
    val newMatrix = matrix.++(0.0)
    assert(newMatrix == Matrix(List(List(1.0, 2.0, 0.0), List(3.0, 4.0, 0.0))))
  }

  test("Linear Regression (30p)") {
    // Expected values
    val expectedResults: List[(Matrix,Option[Double])] = List(
      (Matrix(List(List(0.0), List(0.0))), Some(60000.0)),
      (Matrix(List(List(0.0), List(0.0))), Some(80000.0)),
      (Matrix(List(List(0.0), List(0.0), List(0.0))), Some(50000.0))
    )

    // Compute results using linear regression function
    val computedResults: List[(Matrix,Option[Double])] = List(
      linearRegression(steps, Matrix(List(List(0.0, 0.0)).transpose), List("GrLivArea", "SalePrice")),
      linearRegression(steps, Matrix(List(List(0.0, 0.0)).transpose), List("GarageArea", "SalePrice")),
      linearRegression(steps, Matrix(List(List(0.0, 0.0, 0.0)).transpose), List("GrLivArea", "GarageArea", "SalePrice"))
    )

    // we compare the expected vs achieved square mean error only in tests. We do not test parameters in any way;
    val trueValues: List[Boolean] = {
    for (((_,cerr), (_,eerr)) <- computedResults.zip(expectedResults))
      yield {
        (cerr,eerr) match {
          case (Some(x), Some(y)) => math.abs(x - y) < 10000
          case _ =>  false
        }
      }
    }
    // all comparisons much succeed
    assert(trueValues.foldRight(true)(_ && _))
  }
}

