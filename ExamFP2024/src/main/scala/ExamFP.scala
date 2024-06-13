
object ExamFP{

  def profileID:Int = ???

  /*
  Exercise 1:

  Determine if a given list contains duplicate elements
   */
  def hasDuplicates (l: List[Int]): Boolean = {
    if (l.size != l.toSet.size) true
    else false
  }

  /*
  Exercise 2:

  Write a function which takes a list of integers l = x1, x2, ... xn and a
  list of pairs (i,j) with i < j and returns the list of sublists xi, ..., xj

  It is guaranteed that the indices obey the above conditions.

   */

  def sublists(l: List[Int], indexes: List[(Int,Int)]): List[List[Int]] = {
    indexes.map((start, stop) => l.slice(start, stop))
  }

  /*
  Exercise 3:

  Write a function which takes a matrix of integers and returns the sum of
  all elements from both diagonals. It is guaranteed that the matrix is square.
   */

  def sumDiags(l: List[List[Int]]): Int = {
    val sumFirstDiag = (for (i <- l.indices) yield l(i)(i)).toList.sum
    val sumSndDiag = (for (i <- l.indices) yield l(i)(l.length - 1 - i)).toList.sum
    
    sumFirstDiag + sumSndDiag
        
  }
  /*
  Exercise 4:

  Write a function which returns all palindrome strings from a list:
   */
  def palindromes(l: List[String]): List[String] = {
    def isPalindrome(s: String): Boolean = {
      if(s == s.reverse) true
      else false
    }

    l.filter(isPalindrome)
  }

  /*
  Exercise 5:

  Write a function which takes a polymorphic list of optional values
  and converts them to an optional list of values.
  If the initial list contains at least a None value, then the result
  should be none.

   */
  def loption[A](l: List[Option[A]]): Option[List[A]] = {
    l match {
      case Nil => None
      case x :: xs => x match {
        case None => None
        case Some(value) => loption(xs) match {
          case None => None
          case Some(rest) => Some(value :: rest)
        }
      }
    }
  }

  // A FPTree is a tree which has an arbitrary number of children
  trait FPTree{}
  case object Void extends FPTree
  case class Node(key: Int, children: List[FPTree]) extends FPTree {}

  /*
  Exercise 6:

  Write a function which computes the sum of all elements
  strictly larger than a given x from a FPTree
   */
  def sumCond(x: Int)(t: FPTree): Int = {
    def aux(subtree: FPTree): Int = subtree match {
      case Void => 0
      case Node(key, children) =>
        val sumOfChildren = children.map(aux).sum
        if (key > x) key + sumOfChildren else sumOfChildren
    }

    aux(t)
  }

  /*
  Exercise 7:
  Write a function which removes all elements deeper than height h
  from an FPTree
   */
  def limitHeight(h: Int)(t: FPTree): FPTree = {
    def goDown(counter: Int, subTree: FPTree): FPTree = {
      if(counter > h) Void
      else subTree match {
        case Void => Void
        case Node(key, children) => val newChildren = children.map(child => goDown(counter+1, child)).filter(_ != Void)
        Node(key, newChildren)
      }
    }
    
    goDown(1, t)
  }


  /*
  Exercise 8:
  Write a function which takes an image represented as a matrix of integers
  and a mask (a matrix of true/false values) and applies the mask on the image
  (every pixel corresponding to a true-value is kept, all others are set to zero).

   It is guaranteed that the image and its mask are non-empty and have the same dimensions
   */

  def applyMask(img: List[List[Int]], mask: List[List[Boolean]]): List[List[Int]] = {
    img.zip(mask).map { case (imgRow, maskRow) =>
      imgRow.zip(maskRow).map { case (pixel, maskValue) =>
        if (maskValue) pixel else 0
      }
    }
  }

  /* Exercise 9:
  Write a function which takes two lists and returns a list of elements
  x such that x is a member of l1 but not l2, or x is a member of l2 but not l1
   */

  def unique_union(l1: List[Int], l2: List[Int]): List[Int] = {
    val s1 = l1.toSet
    val s2 = l2.toSet
    val union = s1 union s2
    val intersect = s1 intersect s2
    (union diff intersect).toList




  }
  /*
  Exercise 10:
  A gradebook is encoded as a list of triples: (StudentName, Lecture, Grade).
  You must establish if there is some student which has failed all the lectures
  he has taken.
   */

  def someFail(gradebook: List[(String, String, Int)]): Boolean = {
    // Group the gradebook by student name
    val groupedByStudent = gradebook.groupBy(_._1)

    // Check if any student has failed all their lectures
    groupedByStudent.exists { case (student, grades) =>
      grades.forall(_._3 < 50) // Check if all grades are below 50
    }
  }



  /*
  Exercise 11:
  Write a function which takes two lists l1 = x1, x2, ..., xn
  and l2 = y1, y2, ..., ym
  and returns the list x1,y1, x2, y2, ... xn, yn, ... ym if n <= m or
   x1,y1, x2, y2, ... xm, ym, ... xn if n > m
   */

  def join(l1: List[Int], l2: List[Int]): List[Int] = {

    val commonIndex = l1 zip l2 flatMap { (i1, i2) => List(i1,i2)}
    if (l1.length > l2.length) commonIndex ++ l1.drop(l2.length)
    else commonIndex ++ l2.drop(l1.length)

  }

  /*
  Exercise 12:
  Write a function which takes a map which assigns a list of grades to each lecture
  and returns the highest grade obtained at an any lecture from the map
   */
  def mapmax(m: Map[String,List[Int]]): Int = {
    m.values.flatten.max
  }

  /*
  Exercise 13:
  Write a function which removes ONLY consecutive occurrences of a value
  from a list.
   */

  def removeConsecutive(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case x :: Nil => List(x)
    case x :: xs => if ( x == xs.head ) removeConsecutive(xs)
                    else x :: removeConsecutive(xs)
  }

  /*
  Exercise 14: Write a function which takes an integer k, a list of
               size n, and returns the last k elements from the list.
               You CANNOT use take, drop or split.

               It is guaranteed that n >= k.
   */
  def intersect(f1 : Map[Int,Int], f2: Map[Int,Int], range:(Int,Int)): Boolean = ???
}