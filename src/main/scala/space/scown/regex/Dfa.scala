package space.scown.regex

import space.scown.regex.Dfa.TransitionMap

case class Dfa(startState: State, finalStates: Set[State], transitions: TransitionMap) {

}

object Dfa {
  type TransitionMap = Map[State, Map[MatchedCharacter, State]]
}
