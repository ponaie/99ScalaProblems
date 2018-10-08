// P25 (*) Generate a random permutation of the elements of a list.
// Hint: Use the solution of problem P23.
// Example:
//
// scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
// res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)

object P25 {
  import P23.randomSelect

  // O(n2)
  def randomPermute[A](ls: List[A]): List[A] =
    randomSelect(ls.length, ls)

  // Fisher-Yates, O(n)
  def randomPermute[A](ls: List[A]): List[A] = {
    val rand = new util.Random
    val a = ls.toArray
    for (i <- a.length - 1 to 1 by -1) {
      val i1 = rand.nextInt(i + 1)
      val t = a(i)
      a.update(i, a(i1))
      a.update(i1, t)
    }
    a.toList
  }
}
