package space.scown.regex

import org.scalatest.FunSpec
// exclude Predef to avoid conflicts
import scala.Predef.{augmentString => _}
import scala.Predef.ArrowAssoc
import scala.collection.immutable.{Set, Map}
import space.scown.regex.Nfa.string2Nfa
import scala.language.postfixOps

class Nfa$Test extends FunSpec {

  describe("Nfa") {
    it("should support the empty string") {
      val nfa: Nfa = ""

      assert(nfa == Nfa(
        new State(0),
        Set(new State(1)),
        Map(
          new State(0) -> Map((Epsilon, Set(new State(1))))
        )
      ))
    }

    it("should support single characters") {
      val nextInt = State.labels.next()
      val startIndex = nextInt + 1
      val nfa: Nfa = "0"

      assert(nfa == Nfa(
        new State(startIndex),
        Set(new State(startIndex + 1)),
        Map(
          new State(startIndex) -> Map((MatchedCharacter('0'), Set(new State(startIndex + 1))))
        )
      ))
    }

    it("should support concatenation") {
      val nextInt = State.labels.next()
      val startA = nextInt + 1
      val endA = nextInt + 2
      val startB = nextInt + 3
      val endB = nextInt + 4
      val start = nextInt + 5
      val join = nextInt + 6
      val finalState = nextInt + 7

      val nfa: Nfa = "ab"

      assert(nfa == Nfa(
        new State(start),
        Set(new State(finalState)),
        Map(
          new State(start) -> (Map() + (Epsilon -> Set(new State(startA)))),
          new State(endA) -> (Map() + (Epsilon -> Set(new State(join)))),
          new State(join) -> (Map() + (Epsilon -> Set(new State(startB)))),
          new State(endB) -> (Map() + (Epsilon -> Set(new State(finalState)))),
          new State(startA) -> (Map() + (MatchedCharacter('a') -> Set(new State(endA)))),
          new State(startB) -> (Map() + (MatchedCharacter('b') -> Set(new State(endB))))
        )
      ))
    }

    it("should support unions") {
      val nextInt = State.labels.next()
      val startA = nextInt + 1
      val endA = nextInt + 2
      val startB = nextInt + 3
      val endB = nextInt + 4
      val start = nextInt + 5
      val finalState = nextInt + 6

      val nfa: Nfa = "a"|"b"

      assert(nfa == Nfa(
        new State(start),
        Set(new State(finalState)),
        Map(
          new State(start) -> (Map() + (Epsilon -> Set(new State(startA), new State(startB)))),
          new State(endA) -> (Map() + (Epsilon -> Set(new State(finalState)))),
          new State(endB) -> (Map() + (Epsilon -> Set(new State(finalState)))),
          new State(startA) -> (Map() + (MatchedCharacter('a') -> Set(new State(endA)))),
          new State(startB) -> (Map() + (MatchedCharacter('b') -> Set(new State(endB))))
        )
      ))
    }

    it("should support *") {
      val nextInt = State.labels.next()
      val startA = nextInt + 1
      val endA = nextInt + 2
      val start = nextInt + 3
      val finalState = nextInt + 4
      val startLoop = nextInt + 5
      val endLoop = nextInt + 6

      val nfa: Nfa = "a"*

      assert(nfa == Nfa(
        new State(start),
        Set(new State(finalState)),
        Map(
          new State(start) -> (Map() + (Epsilon -> Set(new State(startLoop), new State(finalState)))),
          new State(endLoop) -> (Map() + (Epsilon -> Set(new State(startLoop), new State(finalState)))),
          new State(startLoop) -> (Map() + (Epsilon -> Set(new State(startA)))),
          new State(endA) -> (Map() + (Epsilon -> Set(new State(endLoop)))),
          new State(startA) -> (Map() + (MatchedCharacter('a') -> Set(new State(endA))))
        )
      ))
    }

    describe("compile") {
      it("should compile the empty regex") {
        val start = 0
        val dfaState = s"$start"

        val nfa: Nfa = ""

        assert(nfa.compile == Dfa(
          State(dfaState),
          Set(State(dfaState)),
          Map()
        ))
      }

      it("should compile the single character regex") {
        val nextInt = State.labels.next()
        val start = nextInt + 1
        val finalState = nextInt + 2

        val nfa: Nfa = "0"

        assert(nfa.compile == Dfa(
          new State(start),
          Set(new State(finalState)),
          Map((new State(start), Map((MatchedCharacter('0'), new State(finalState)))))
        ))
      }

      it("should compile a concatenation regex") {
        val nextInt = State.labels.next()
        val endA = nextInt + 2
        val endB = nextInt + 4
        val start = nextInt + 5

        val nfa: Nfa = "ab"

        assert(nfa.compile == Dfa(
          new State(s"$start"),
          Set(new State(s"$endB")),
          Map(
            (new State(s"$start"), Map(('a', new State(s"$endA")))),
            (new State(s"$endA"), Map(('b', new State(s"$endB"))))
          )
        ))
      }

      it("should compile a union regex") {
        val nextInt = State.labels.next()
        val startA = nextInt + 1
        val endA = nextInt + 2
        val startB = nextInt + 3
        val endB = nextInt + 4
        val start = nextInt + 5

        val nfa: Nfa = "a"|"b"

        assert(nfa.compile == Dfa(
          new State(s"$start"),
          Set(new State(s"$endA"), new State(s"$endB")),
          Map(
            (new State(s"$start"), Map(('a', new State(s"$endA")), ('b', new State(s"$endB"))))
          )
        ))
      }

      it("should compile a star regex") {
        val nextInt = State.labels.next()
        val endA = nextInt + 2
        val start = nextInt + 3

        val nfa: Nfa = "a"*

        assert(nfa.compile == Dfa(
          new State(s"$start"),
          Set(new State(s"$start"), new State(s"$endA")),
          Map(
            (new State(s"$start"), Map(('a', new State(s"$endA")))),
            (new State(s"$endA"), Map(('a', new State(s"$endA"))))
          )
        ))
      }
    }
  }

}
