/**
 * Created by apple on 2015-06-25.
 */
object Scala99Solutions {

  def isPalindrome[T](list: List[T]): Boolean = {
    val firstPart: List[T] = list.take(list.size / 2)
    val secondPart: List[T] = list.takeRight(list.size / 2).reverse
    firstPart == secondPart
  }

}
