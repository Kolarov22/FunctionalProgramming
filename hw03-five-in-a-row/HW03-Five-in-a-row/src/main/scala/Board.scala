case class Board(val board: List[List[Player]], val player: Player) {

  type Line = List[Player]

  def isFree(x: Int, y: Int): Boolean = board(x)(y) match {
    case Empty => true
    case _ => false
  }

  def getColumns: Board =
    Board(board.transpose, player)
  def getFstDiag: Line = {
    (for (i <- board.indices) yield board(i)(i)).toList
  }
  def getSndDiag: Line = {
    (for (i <- board.indices) yield board(i)(board.length - 1 - i)).toList
  }

  def getAboveFstDiag: List[Line] = {
    val size = board.length
    (1 until size).map { k =>
      (0 until size - k).map { i =>
        board(i)(i+k)
      }.toList
    }.toList
  }


  def getBelowFstDiag: List[Line] = {
     getColumns.getAboveFstDiag

  }
  def getAboveSndDiag: List[Line] = {
    val size = board.length
    (1 until size).map { k =>
      (0 until size - k).map { i =>
        board(i)(size - 1 - (i + k))
      }.toList
    }.toList
  }
  def getBelowSndDiag: List[Line] = {
    val size = board.length
    (1 until size).map { k =>
      (0 until size - k).map { i =>
        board(i + k)(size - 1 - i)
      }.toList
    }.toList

  }
  def winner: Boolean = {

     sequences.getOrElse(5, 0) > 0

  }
  def update(ln: Int, col: Int): Board = {
    val updatedRow = List(board(ln).updated(col, player))

    val updatedBoard = board.take(ln) ++ updatedRow ++ board.drop(ln + 1)
    Board(updatedBoard, player)
  }

  def next: List[Board] = {
    for {
      x <- 0 until board.size
      y <- 0 until board(x).size
      if isFree(x, y)
    } yield update(x, y)
  }.toList


  def sequences: Map[Int,Int] = {
    var result = Map(5 -> 0, 4 -> 0, 3 -> 0, 2 -> 0)


    def countSequences(line: Line, player: Player): Map[Int, Int] = {


      def countSegmentsOfLength(length: Int): Int = {
        line.sliding(5).count { segment =>
          segment.count(_ == player) == length && segment.count(_ == Empty) == 5 - length
        }
      }

      val countOfLength5 = line.sliding(5).count(_.forall(_ == player))
      
      val countsOfLengths234 = (2 to 4).map(length => length -> countSegmentsOfLength(length)).toMap

      Map(5 -> countOfLength5) ++ countsOfLengths234
    }


    var lines = List[Line]()
    lines ++= board
    lines ++= getColumns.board
    lines ++= List(getFstDiag)
    lines ++= List(getSndDiag)
    lines ++= getAboveFstDiag
    lines ++= getBelowFstDiag
    lines ++= getAboveSndDiag
    lines ++= getBelowSndDiag


    for (line <- lines) {
      val counts = countSequences(line, player)
      for (length <- result.keys) {
        result = result.updated(length, result(length) + counts(length))
      }
    }

    result

  }

  override def toString: String = {
    def toChar(p: Player): Char = p match {
      case One => 'X'
      case Two => '0'
      case Empty => '.'
    }

    board.map(row => row.map(toChar).mkString).mkString("\n")
}
}

object Board {

  def profileID:Int = 525355

  def apply(s: String, p: Player): Board = {
      def toPos(c: Char): Player = {
        c match {
          case 'X' => One
          case '0' => Two
          case _ => Empty
        }
      }

    val rows = s.split('\n').toList.map(line => line.toList.map(toPos))
      Board(rows, p)
  }

  def apply(s: String): Board = {
    def toPos(c: Char): Player =
      c match {
        case 'X' => One
        case '0' => Two
        case _ => Empty
      }
    val rows = s.split('\n').toList.map(line => line.toList.map(toPos))
    Board(rows, One)
  }
}
