package space.scown.regex

import space.scown.regex.Nfa.TransitionMap

import scala.languageFeature.implicitConversions

sealed trait Matched

case object Epsilon extends Matched
case class MatchedCharacter(c: Char) extends Matched

case class Nfa(startState: State, finalStates: Set[State], transitions: TransitionMap) {
  def apply(other: Nfa): Nfa = Nfa.concatenation(this, other)
  def |(other: Nfa): Nfa = Nfa.union(this, other)
  def * : Nfa = Nfa.star(this)
}

// Represent as a Map[State, Map[Single, Set[State]]], with a startState and a set of final states
// Need States to generate
object Nfa {
  type TransitionMap = Map[State, Map[Matched, Set[State]]]

  val EMPTY: Nfa = character(Epsilon)

  def character(c: Matched): Nfa = {
    val startState = new State()
    val finalState = new State()

    Nfa(
      startState,
      Set(finalState),
      Map(
        startState -> Map(c -> Set(finalState))
      )
    )
  }

  def concatenation(n1: Nfa, n2: Nfa): Nfa = {
    val startState = new State()
    val joinState = new State()
    val finalState = new State()


    Nfa(
      startState,
      Set(finalState),
      Map(
        startState -> Map(Epsilon -> Set(n1.startState)),
        joinState -> Map(Epsilon -> Set(n2.startState))
      ).asInstanceOf[TransitionMap]
        ++ n1.finalStates.map(s => (s, Map(Epsilon.asInstanceOf[Matched] -> Set(joinState))))
        ++ n2.finalStates.map(s => (s, Map(Epsilon.asInstanceOf[Matched] -> Set(finalState)))).toMap
        ++ n1.transitions
        ++ n2.transitions
    )
  }

  def union(n1: Nfa, n2: Nfa): Nfa = {
    val startState = new State()
    val finalState = new State()


    Nfa(
      startState,
      Set(finalState),
      Map(
        startState -> Map(Epsilon -> Set(n1.startState, n2.startState))
      ).asInstanceOf[TransitionMap]
        ++ n1.finalStates.map(s => (s, Map(Epsilon.asInstanceOf[Matched] -> Set(finalState))))
        ++ n2.finalStates.map(s => (s, Map(Epsilon.asInstanceOf[Matched] -> Set(finalState))))
        ++ n1.transitions
        ++ n2.transitions
    )
  }

  def star(n: Nfa): Nfa = {
    val startState = new State()
    val finalState = new State()
    val startLoop = new State()
    val endLoop = new State()

    Nfa(
      startState,
      Set(finalState),
      Map(
        startState -> Map(Epsilon -> Set(startLoop, finalState)),
        startLoop -> Map(Epsilon -> Set(n.startState)),
        endLoop -> Map(Epsilon -> Set(startLoop, finalState))
      ).asInstanceOf[TransitionMap]
        ++ n.finalStates.map(s => (s, Map(Epsilon.asInstanceOf[Matched] -> Set(endLoop))))
        ++ n.transitions
    )
  }

  implicit def string2Nfa(s: String): Nfa = {
    if (s.isEmpty) Nfa.EMPTY
    else s.map(c => Nfa.character(MatchedCharacter(c))).reduce(Nfa.concatenation)
  }

}
