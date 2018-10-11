/*
 * Copyright 2018 Alex Scown
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package space.scown.regex

import space.scown.regex.Dfa.TransitionMap

import scala.annotation.tailrec

case class Dfa(startState: State, finalStates: Set[State], transitions: TransitionMap) {
  def matches(s: String): Stream[String] = {
    @tailrec
    def helper(s: Vector[Char], state: State, lastResult: Option[String], acc: Vector[Char], next: Vector[Char], stream: Stream[String]): Stream[String] = s match {
      // If there is no more input, return the last result if we have one
      case Vector() if lastResult.isDefined => stream #::: Stream(lastResult.get)
      case Vector() => stream
      case h +: t => transitions.getOrElse(state, Map()).get(h) match {
        // If the next state is final, record the result
        case Some(nextState) if finalStates.contains(nextState) => helper(t, nextState, Some((acc :+ h).mkString("")), acc :+ h, t, stream)
        // If the next state is not final, build the accumulator
        case Some(nextState) => helper(t, nextState, lastResult, acc :+ h, next, stream)
        // If there is no next state, return a Stream containing the last result if we have one
        case None if lastResult.isDefined => helper(next, startState, None, Vector(), Vector(), stream #::: Stream(lastResult.get))
        case None => helper(t, startState, None, Vector(), Vector(), stream)
      }
    }

    helper(s.toVector, startState, None, Vector(), Vector(), Stream())
  }
}

object Dfa {
  type TransitionMap = Map[State, Map[Char, State]]
}
