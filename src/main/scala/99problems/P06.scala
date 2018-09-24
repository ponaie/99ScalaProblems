// P06 (*) Find out whether a list is a palindrome.
// Example:
// scala> isPalindrome(List(1, 2, 3, 2, 1))
// res0: Boolean = true

object P06 {
  // Builtin
  def isPalindrome[A](list: List[A]): Boolean = list == list.reverse
}
