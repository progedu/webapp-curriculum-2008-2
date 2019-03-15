object Main {

  implicit class RichList[T](val list: List[T]) {

    import scala.annotation.tailrec
    import scala.util.Random

    /**
      * リストのN番目の要素を削除
      * @param n
      * @param list
      * @tparam T
      * @return (N番目の要素を除いたリスト, 削除された要素)
      */
    private[this] def removeAt[T](n: Int, list: List[T]): (List[T], T) =  {
      @tailrec
      def go(n: Int, list: List[T], acc : List[T]): (List[T], T) = (n, list) match {
        case (0, x :: xs) => (acc.reverse ::: xs, x)
        case (_, x :: xs) => go(n - 1, xs, x :: acc)
      }
      go(n, list, Nil)
    }

    /**
      * リストからN個の要素をランダムに抽出
      * @param n
      * @param list
      * @tparam T
      * @return 抽出した要素のリスト
      */
    private[this] def randomSelect[T](n: Int, list: List[T]): List[T] = {
      @tailrec
      def go(n: Int, list: List[T], acc: List[T]): List[T] = (n, list) match {
        case (0, _) => acc.reverse
        case (_, l) =>
          val (rest, e) = removeAt(Random.nextInt(l.length), l)
          go(n - 1, rest, e :: acc)
      }
      go(n, list, Nil)
    }

    /**
      * リストの要素をランダムに並べ替える
      * @return
      */
    def shuffle: List[T] = randomSelect(list.length, list)

  }
}
