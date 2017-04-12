object Main {

  implicit class RichList[T](val list: List[T]) {
    def compress: List[T] = {
      def compressRecursive(li: List[T], pressed: List[T]): List[T] = li match {
        case Nil => pressed
        case x :: xs =>
          if (pressed contains x) compressRecursive(xs, pressed)
          else compressRecursive(xs, pressed ++ List(x))
      }
      compressRecursive(list, List())
    }
  }

}
