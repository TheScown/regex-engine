package space.scown.regex

case class State(label: String) {

  def this(i: Int) = this(i.toString)

  def this() = this(State.labels.next())

  def this(labels: Set[String]) = this(labels.toSeq.sorted.mkString(","))

}

private object State {
  val labels: Iterator[Int] = Stream.iterate(0)(s => s + 1).iterator
}