package space.scown.regex

sealed trait Matched

case object Epsilon extends Matched
case class MatchedCharacter(c: Char) extends Matched


case class Nfa(startState: State, finalState: State, states: Set[State]) {
}

object Nfa {
  private val EMPTY = character(Epsilon)
  private val TERMINAL = State(Map(), isFinal = true)

  def apply(startState: State, finalState: State): Nfa = Nfa(startState, finalState, Set(startState, finalState))

  def character(c: Matched): Nfa = {
    Nfa(
      State(transitions = Map(
        c -> Set(TERMINAL)
      )),
      TERMINAL
    )
  }

}

private case class State(transitions: Map[Matched, Set[State]], isFinal: Boolean = false) {
  private def +(c: MatchedCharacter, ss: Set[State]): State = transitions.get(c) match {
    case Some(set) => copy(transitions + (c -> ss.union(set)))
    case None => copy(transitions + (c -> ss))
  }
}
