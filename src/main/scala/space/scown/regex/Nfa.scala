package space.scown.regex

import scala.languageFeature.implicitConversions

sealed trait Matched

case object Epsilon extends Matched
case class MatchedCharacter(c: Char) extends Matched


case class Nfa(startState: State, finalStates: Set[State], transitions: Map[State, Map[Matched, Set[State]]]) {
  def apply(other: Nfa): Nfa = Nfa.concatenation(this, other)
  def |(other: Nfa): Nfa = Nfa.union(this, other)
  def * : Nfa = Nfa.star(this)
}

// Represent as a Map[State, Map[Single, Set[State]]], with a startState and a set of final states
// Need States to generate
object Nfa {
  val EMPTY: Nfa = character(Epsilon)

  def character(c: Matched): Nfa = {
    val startState = State()
    val finalState = State()

    Nfa(
      startState,
      Set(finalState),
      Map(
        startState -> Map(c -> Set(finalState))
      )
    )
  }

  def concatenation(n1: Nfa, n2: Nfa): Nfa = {
    val startState = State()
    val joinState = State()
    val finalState = State()


    Nfa(
      startState,
      Set(finalState),
      Map(
        startState -> Map(Epsilon -> Set(n1.startState)),
        joinState -> Map(Epsilon -> Set(n2.startState))
      ).asInstanceOf[Map[State, Map[Matched, Set[State]]]]
        ++ n1.finalStates.map(s => (s, Map(Epsilon -> Set(joinState)))).asInstanceOf[Map[State, Map[Matched, Set[State]]]]
        ++ n2.finalStates.map(s => (s, Map(Epsilon -> Set(finalState)))).asInstanceOf[Map[State, Map[Matched, Set[State]]]]
        ++ n1.transitions
        ++ n2.transitions
    )
  }

  def union(n1: Nfa, n2: Nfa): Nfa = {
    val startState = State()
    val finalState = State()


    Nfa(
      startState,
      Set(finalState),
      Map(
        startState -> Map(Epsilon -> Set(n1.startState, n2.startState))
      ).asInstanceOf[Map[State, Map[Matched, Set[State]]]]
        ++ n1.finalStates.map(s => (s, Map(Epsilon -> Set(finalState)))).asInstanceOf[Map[State, Map[Matched, Set[State]]]]
        ++ n2.finalStates.map(s => (s, Map(Epsilon -> Set(finalState)))).asInstanceOf[Map[State, Map[Matched, Set[State]]]]
        ++ n1.transitions
        ++ n2.transitions
    )
  }

  def star(n: Nfa): Nfa = {
    val startState = State()
    val finalState = State()
    val startLoop = State()
    val endLoop = State()

    Nfa(
      startState,
      Set(finalState),
      Map(
        startState -> Map(Epsilon -> Set(startLoop, finalState)),
        startLoop -> Map(Epsilon -> Set(n.startState)),
        endLoop -> Map(Epsilon -> Set(startLoop, finalState))
      ).asInstanceOf[Map[State, Map[Matched, Set[State]]]]
        ++ n.finalStates.map(s => (s, Map(Epsilon -> Set(endLoop)))).asInstanceOf[Map[State, Map[Matched, Set[State]]]]
        ++ n.transitions
    )
  }

  implicit def string2Nfa(s: String): Nfa = {
    if (s.isEmpty) Nfa.EMPTY
    else s.map(c => Nfa.character(MatchedCharacter(c))).reduce(Nfa.concatenation)
  }

}

case class State() {
  val label: String = State.labels.next()

  override def hashCode(): Int = label.hashCode

  override def equals(obj: scala.Any): Boolean = {
    if (!obj.isInstanceOf[State]) {
      false
    }
    else {
      label == obj.asInstanceOf[State].label
    }
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[State]
}

private object State {
  val labels: Iterator[String] = Stream.iterate("A")(s => {
    if (s.last == 'Z') "A" * (s.length + 1)
    else s.substring(0, s.length - 1) + s.last.+(1).toChar
  }).iterator
}
