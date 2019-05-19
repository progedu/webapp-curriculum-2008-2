object Main {
  implicit class RichString(src: String) {
    // 指定の数だけ草を生やせるため、正確に気持ちを表現できる
    // 単芝チェックを行い、単芝うぜぇと言われるのを防止する
    def grass(num: Int): String = {
      require(num > 1 || num == 0, "単芝またはマイナスの草は禁止されています")
      src + "ｗ" * num
    }
  }
}