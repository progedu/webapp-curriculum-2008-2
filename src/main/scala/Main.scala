object Main {
  implicit class RichString(hoge: String) {
    def line = s"---------${hoge}----------"
  }
}
