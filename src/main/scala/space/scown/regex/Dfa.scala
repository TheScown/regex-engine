package space.scown.regex

import space.scown.regex.Dfa.TransitionMap

case class Dfa(startState: State, finalStates: Set[State], transitions: TransitionMap) {
  def matches(s: String): Stream[String] = {
    def helper(s: Vector[Char], state: State, lastResult: Option[String], acc: Vector[Char], next: Vector[Char]): Stream[String] = s match {
      case Vector() if lastResult.isDefined => Stream(lastResult.get)
      case Vector() => Stream()
      case h +: t => transitions.getOrElse(state, Map()).get(h) match {
        case Some(nextState) if finalStates.contains(nextState) => helper(t, nextState, Some((acc :+ h).mkString("")), acc :+ h, t)
        case Some(nextState) => helper(t, nextState, lastResult, acc :+ h, next)
        case None if lastResult.isDefined => lastResult.get #:: helper(next, startState, None, Vector(), Vector())
        case None => helper(t, startState, None, Vector(), Vector())
      }
    }

    helper(s.toVector, startState, None, Vector(), Vector())
  }
}

object Dfa {
  type TransitionMap = Map[State, Map[Char, State]]
}
