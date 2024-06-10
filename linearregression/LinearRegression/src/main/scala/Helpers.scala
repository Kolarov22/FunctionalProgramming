object Helpers {
  def zipWith[A, B, C](op: (A, B) => C)(l1: List[A], l2: List[B]): List[C] = {
    l1.zip(l2).map(pair => op(pair._1, pair._2))
  }

}
