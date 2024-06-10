object FSets{

  type Set = Int => Boolean

  def profileID: Int = 525355

  def member(e: Int)(s: Set): Boolean = {
    if (s(e)) true
    else false
  }

  def singleton(x: Int): Set = {
    (el: Int) => x == el
  }

  def ins(x: Int)(s: Set): Set = {
    (el: Int) => s(el) || x == el
  }

  def fromBounds(start: Int, stop: Int): Set = {
    (el: Int) => (el >= start && el <= stop)
  }

  def union (s1: Set, s2: Set): Set = {
    (el: Int) => (member(el)(s1) || member(el)(s2))
  }

  def complement(s1: Set): Set = {
    (el: Int) => {
      if (member(el)(s1)) false
      else true
    }
  }

  def sumSet(b: Int)(start: Int, stop: Int)(s: Set): Int = {
    def aux(crt: Int, acc: Int): Int = {
      if (crt > stop) acc
      else if (member(crt)(s)) aux(crt+1, acc + crt)
      else aux(crt+1, acc)
    }
    aux(start, b)
  }

  def foldLeftSet(b:Int)(op: (Int,Int) => Int)(start: Int, stop: Int)(s: Set): Int = {
    def aux(crt: Int, acc: Int): Int = {
      if (crt > stop) acc
      else if (member(crt)(s)) aux(crt+1, op(acc, crt))
      else aux(crt+1, acc)
    }
    aux(start, b)
  }

  def foldRightSet(b:Int)(op: (Int,Int) => Int)(start: Int, stop: Int)(s: Set): Int = {
    def rec_fold(crt: Int): Int = {
      if (crt > stop) b
      else if(member(crt)(s)) op(crt, rec_fold(crt + 1))
      else rec_fold(crt + 1)
    }

    rec_fold(start)
  }



  def filter(p: Int => Boolean)(s: Set): Set = {
    (el: Int) => p(el)
  }

  def partition(p: Int => Boolean)(s: Set): (Set,Set) = {
    (filter(p)(s), complement(filter(p)(s)))
  }


  def forall(cond: Int => Boolean)(start: Int, stop: Int)(s: Set): Boolean = {
    def aux(crt: Int, status: Boolean): Boolean = {
      if (crt > stop) status
      else if(!member(crt)(s)) aux(crt+1, status)
      else if (cond(crt)) aux(crt+1, status)
      else false
    }

    aux(start, true)
  }

  def exists(cond: Int => Boolean)(start: Int, stop: Int)(s: Set): Boolean = {
    def loop(crt: Int, status: Boolean): Boolean = {
      if (crt > stop) status
      else if (member(crt)(s) && cond(crt)) true
      else loop(crt+1, false)
    }
    loop(start, false)
  }

  def setOfDivByK(k: Int): Set = {
    (el: Int) => (el % k == 0)
  }

  def moreDivs(k: Int)(start: Int, stop:Int)(s1: Set, s2: Set): Boolean = {
   def aux(crt: Int, counterS1: Int, counterS2: Int): Boolean = {
     if (crt > stop) (counterS1 > counterS2)
     else if (member(crt)(s1) && crt % k == 0) aux(crt+1, counterS1+1, counterS2)
     else if (member(crt)(s2) && crt % k == 0) aux(crt+1, counterS1, counterS2+1)
     else aux(crt+1, counterS1, counterS2)
   }

    aux(start, 0, 0)
  }

}
