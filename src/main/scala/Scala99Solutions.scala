/**
 * Created by apple on 2015-06-25.
 */
object Scala99Solutions {

  //06:Find out whether a list is a palindrome.
  /*
  Example:
  scala> isPalindrome(List(1, 2, 3, 2, 1))
  res0: Boolean = true
   */
  def isPalindrome[T](list: List[T]): Boolean = {
    val firstPart: List[T] = list.take(list.size / 2)
    val secondPart: List[T] = list.takeRight(list.size / 2).reverse
    firstPart == secondPart
  }

  //07:Flatten a nested list structure.
  /*
  Example:
  scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  res0: List[Any] = List(1, 1, 2, 3, 5, 8)
   */
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  //08:Eliminate consecutive duplicates of list elements.
  /*
  If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
  Example:
  scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   */
  def compress[T](ls: List[T]): List[T] = ls match {
    case Nil => Nil
    case h :: tail => h +: compress(tail.dropWhile(x => x == ls.head))
  }

  def compressFunctional[T](ls: List[T]): List[T] =
    ls.foldLeft(List[T]()) { (r, h) =>
      if (r.isEmpty || r.last != h) r :+ h
      else r
    }

  //09: Pack consecutive duplicates of list elements into sublists.
  /*
  If a list contains repeated elements they should be placed in separate sublists.
  Example:
  scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
   */
  def pack[T](ls: List[T]): List[List[T]] =
  ls.foldLeft(List[List[T]]()){(result, eachValue) =>
    if(result.isEmpty || result.last.last != eachValue) result :+ List(eachValue)
    else result.init :+ (result.last :+ eachValue)
  }

  /*
  10:Run-length encoding of a list.
  Example:
  scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */
  def encode[T](list: List[T]): List[(Int,T)] = {
    val packedList = pack(list)
    packedList.map(eachList => (eachList.size, eachList.head))
  }
}
