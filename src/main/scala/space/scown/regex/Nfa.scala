package space.scown.regex

import space.scown.regex.Nfa.TransitionMap

import scala.annotation.tailrec
import scala.languageFeature.implicitConversions

sealed trait Matched

case object Epsilon extends Matched
case class MatchedCharacter(c: Char) extends Matched

case class Nfa(startState: State, finalStates: Set[State], transitions: TransitionMap) {
  def apply(other: Nfa): Nfa = Nfa.concatenation(this, other)
  def |(other: Nfa): Nfa = Nfa.union(this, other)
  def * : Nfa = Nfa.star(this)

  def compile: Dfa = {
    @tailrec
    def findReachableStates(states: Set[State], visited: Set[State] = Set(), reachableStates: Set[State] = Set()): Set[State] = {
      if (states.isEmpty) reachableStates
      else {
        val reachable = states.flatMap(s => transitions.getOrElse(s, Map()).getOrElse(Epsilon, Set())).diff(visited)

        findReachableStates(reachable, visited ++ states, states ++ reachable ++ reachableStates)
      }
    }

    def getNfaTransitions(reachableStates: Set[State]) = {
      reachableStates
        .map(s => transitions.getOrElse(s, Map()).filter({
          case (matched, _) => matched != Epsilon
        }).asInstanceOf[Map[MatchedCharacter, Set[State]]])
        .fold(Map())((map1: Map[MatchedCharacter, Set[State]], map2: Map[MatchedCharacter, Set[State]]) => {
          val keys1 = map1.keySet
          val keys2 = map2.keySet

          val intersection = keys1.intersect(keys2)
          val only1 = keys1.diff(keys2)
          val only2 = keys2.diff(keys1)

          (Map()
            ++ only1.map(key => (key, map1(key)))
            ++ only2.map(key => (key, map2(key)))
            ++ intersection.map(key => (key, map1(key) ++ map2(key))))
        })
    }

    def constructDfa(
      nfaStates: Set[State],
      dfa: Dfa,
      stateMap: Map[Set[State], State]
    ): Dfa = {
      val reachableStates = findReachableStates(nfaStates)

      val nfaTransitions: Map[MatchedCharacter, Set[State]] = getNfaTransitions(reachableStates)

      val newDfaStates = nfaTransitions.map({
        case (_, states) => (states, new State(states.map(_.label)))
      }) -- stateMap.keys

      val newTransitions: Map[Char, State] = nfaTransitions.map({
        case (matched, states) => (matched.c, newDfaStates.getOrElse(states, stateMap(states)))
      })

      val dfaState = stateMap(nfaStates)

      val isFinalState = reachableStates.intersect(finalStates).nonEmpty

      val updatedDfa = dfa.copy(
        finalStates = if (isFinalState) dfa.finalStates + dfaState else dfa.finalStates,
        transitions = if (newTransitions.nonEmpty) dfa.transitions + (dfaState -> newTransitions) else dfa.transitions
      )

      if (newDfaStates.isEmpty) updatedDfa
      else newDfaStates
        .map({
          case (states, _) => constructDfa(
            states,
            updatedDfa,
            stateMap ++ newDfaStates
          )
        })
        .fold(dfa)((dfa1, dfa2) => {
          dfa1.copy(
            finalStates = dfa1.finalStates ++ dfa2.finalStates,
            transitions = {
              val intersection = dfa1.transitions.keySet.intersect(dfa2.transitions.keySet)

              val intersectionMap = intersection.map(state => {
                state -> (dfa1.transitions(state) ++ dfa2.transitions(state))
              })

              dfa1.transitions ++ dfa2.transitions ++ intersectionMap
            }
          )
        })
    }

    val nfaStartStates = Set(startState)
    val dfaStartState = new State(nfaStartStates.map(_.label))

    constructDfa(
      nfaStartStates,
      Dfa(dfaStartState, Set(), Map()),
      Map(nfaStartStates -> dfaStartState))
  }
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
