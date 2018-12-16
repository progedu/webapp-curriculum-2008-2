object Main {
  implicit class CutStrings(val src: String) {
    def cut(n: Int): String = {
      def cutRec(index: Int, part: String): String = {
        if (index == n || index > src.size -1) part
        else cutRec(index + 1, part + src(index))
      }
      cutRec(0, "")
    }
  }

}
