import ExamFP._

class Tests extends munit.FunSuite {

  test("Valid profile id:" + profileID) {
    assert(profileID > 0)
  }

  test("E1: hasDuplicates (1p)")
  {
    assert(!hasDuplicates(List(1,2,3,4,5,6,7,8,9)))
    assert(hasDuplicates(List(1,2,3,4,5,6,7,8,1)))
    assert(hasDuplicates(List(1,2,3,4,5,6,7,8,5)))
    assert(!hasDuplicates(Nil))

  }

  test("E2: sublists (1p)"){
    assert(sublists(List(1,2,3,4,5,6,7,8,9,10,11,12,13), List((0,12))) == List(List(1,2,3,4,5,6,7,8,9,10,11,12,13)))
    assert(sublists(List(1,2,3,4,5,6,7,8,9,10,11,12,13), List((3,6),(5,5))) == List(List(4,5,6,7), List(6)))

  }

  test("E3: sumDiags (1p)"){
    assert(sumDiags(List(
      List(1,2,3,4,5,6),
      List(1,2,3,4,5,6),
      List(1,2,3,4,5,6),
      List(1,2,3,4,5,6),
      List(1,2,3,4,5,6),
      List(1,2,3,4,5,6))) == 42)
  }

  test("E4: palindromes (1p)"){
    assert(palindromes(List("amam","aba","abba", "babbabab", "ababababababa")) ==
      List("aba","abba", "ababababababa"))
  }

  test("E5: loption (1p)"){
    assert(loption(List(Some(1),Some(2),None, Some(3))) == None)
    assert(loption(List(Some("A"), Some("B"), Some("C"))) == Some(List("A","B","C")))
  }

  val t1 = Node(9,List(1,2,3,4,5,6,7).map(Node(_,Nil)))
  val t2 = Node(6,List(t1, Node(1, Nil), Node(7, List(Node(0,Nil), Node(10, Nil), t1))))

  test("E6: sumCond (1p)"){

    assert(sumCond(99)(Void) == 0)
    assert(sumCond(4)(t1) == 27)

    assert(sumCond(5)(t2) == 67)
  }

  test("E7: limitHeight (1p)"){
    assert(limitHeight(0)(t1) == Void)
    assert(limitHeight(1)(t1) == Node(9,List(Void,Void,Void,Void,Void,Void,Void)))
    //println(limitHeight(3)(t2))
    assert(limitHeight(3)(t2) == Node(6,List(limitHeight(2)(t1), Node(1, Nil), Node(7, List(Node(0, Nil), Node(10,Nil), limitHeight(1)(t1))))))


  }

  test("E8: applyMask (1p)"){
    val img = List(
      List(1,2,3,4,5,6,7,8,9),
      List(1,2,3,4,5,6,7,8,9),
      List(1,2,3,4,5,6,7,8,9),
      List(1,2,3,4,5,6,7,8,9),
      List(1,2,3,4,5,6,7,8,9))
    val mask = List(
      List(false,false,false,false,false,false,false,false,false),
      List(false, false, false, false, false, false, false, false, false),
      List(false, true, true, false, false, true, true, false, false),
      List(false, true, true, false, false, true, true, false, false),
      List(false, true, true, false, false, true, true, false, false),
    )
    val res = List(
      List(0, 0, 0, 0, 0, 0, 0, 0, 0),
      List(0, 0, 0, 0, 0, 0, 0, 0, 0),
      List(0, 2, 3, 0, 0, 6, 7, 0, 0),
      List(0, 2, 3, 0, 0, 6, 7, 0, 0),
      List(0, 2, 3, 0, 0, 6, 7, 0, 0)
    )

    assert(applyMask(img,mask) == res)
  }

  test("E9: unique_union (1p)"){
    //println(unique_union(List(1,2,9,3,4), List(1,2,3,4,5,6)))
    assert(unique_union(List(1,2,9,3,4), List(1,2,3,4,5,6)).toSet == Set(9,5,6))
    assert(unique_union(List(1,2,3,4,5), List(1,2,3,4,5,6)) == List(6))
    assert(unique_union(List(1,2,3,4,5), List(5,4,3,2,1)) == Nil)
  }

  test("E10: someFail (1p)"){
    val g1 = List(("Mike","FP",9),("Anne","PP",7),("Mike", "PA", 4), ("Joe", "PP", 3))
    assert(someFail(g1))
    val g2 = List(("Mihai", "PA", 4), ("Mihai", "FP", 2), ("Mihai", "AA", 1), ("Mihai", "LFA", 3))
    assert(someFail(g2))
    val g3 = List(("Mike", "FP", 9), ("Anne", "PP", 7), ("Mike", "PA", 4), ("Joe", "PP", 3), ("Joe", "LFA", 10))
    assert(!someFail(g3))

  }

  test("E11: join (1p)"){
    val l1 = List(1,3,5,7,9)
    val l2 = List(2,4,6,8,10,11,12)
    assert(join(l1,l2) == List(1,2,3,4,5,6,7,8,9,10,11,12))

    val l3 = List(1, 3, 5, 7, 9, 11,12,13,14)
    val l4 = List(2, 4, 6, 8, 10)
    assert(join(l3, l4) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,13,14))

  }

  test("E12: mapmax (1p)"){
    assert(mapmax(Map("PP" -> List(3,5,6,9), "AA" -> List(4,5,6), "LFA" -> List(1,9,2), "PA" -> List(9,3,2,5))) == 9)
  }

  test("E13: removeConsecutive (1p)"){
    assert(removeConsecutive(List(1,2,3,4,5)) == List(1,2,3,4,5))
    assert(removeConsecutive(List(1,1,2,3,3,4,4,4,4,5,5,5,6,6)) == List(1,2,3,4,5,6))
    assert(removeConsecutive(List(1,1,2,3,3,4,4,4,4,5,5,5,6,6,1,1,2,2)) == List(1,2,3,4,5,6,1,2))
  }

  test("E14: intersect (1p)"){
    val f1 = Map (1 -> 2, 3 -> 4, 5 -> 6, 7 -> 8, 8 -> 9) // y = x+1
    val f2 = Map (1 -> 7, 3 -> 7, 5 -> 6, 7 -> 7, 8 -> 8) // concave

    assert(intersect(f1,f1,(0,10)))
    assert(intersect(f1,f2,(0,10)))
    assert(!intersect(f1,f2,(0,3)))
    assert(!intersect(f1,f2,(6,10)))
  }




}
