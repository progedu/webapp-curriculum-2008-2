import scala.collection.MapView

object Main {

  implicit class TallyList[A](val src: List[A]) {
    def tally: MapView[A, Int] = {
      src.groupBy(identity).mapValues(_.size)
    }
  }

  println(List(1, 2, 2).tally) // Map(2 -> 2, 1 -> 1)
}
