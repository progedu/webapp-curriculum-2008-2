object Main {

}

implicit class ExtendNums[T >: AnyVal](val value: T) {
  def sqrt: T = value match {
    case x: Double => Math.sqrt(x)
    case x: Int => Math.sqrt(x.toDouble).toInt
    case x: Long => Math.sqrt(x.toDouble).toLong
  }
}
