package space.scown.regex

import space.scown.regex.Dfa.TransitionMap

case class Dfa(startState: State, finalStates: Set[State], transitions: TransitionMap) {
  def matches(s: String): Option[MatchResult] = {
    def helper(s: Vector[Char], state: State, lastResult: Option[String], acc: Vector[Char], next: Vector[Char]): Option[MatchResult] = s match {
      case Vector() if lastResult.isDefined => Some(MatchResult(lastResult.get, () => None))
      case Vector() => None
      case h +: t => transitions.getOrElse(state, Map()).get(h) match {
        case Some(nextState) if finalStates.contains(nextState) => helper(t, nextState, Some((acc :+ h).mkString("")), acc :+ h, t)
        case Some(nextState) => helper(t, nextState, lastResult, acc :+ h, next)
        case None if lastResult.isDefined => Some(MatchResult(lastResult.get, () => helper(next, startState, None, Vector(), Vector())))
        case None => helper(t, startState, None, Vector(), Vector())
      }
    }

    helper(s.toVector, startState, None, Vector(), Vector())
  }
}

object Dfa {
  type TransitionMap = Map[State, Map[Char, State]]
}

case class MatchResult(matched: String, private val _next: () => Option[MatchResult]) {
  def next(): Option[MatchResult] = _next()
}
