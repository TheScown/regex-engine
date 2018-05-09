package space.scown.regex

import org.scalatest.FunSpec
// exclude Predef to avoid conflicts
import scala.Predef.{augmentString => _}
import scala.Predef.ArrowAssoc
import scala.collection.immutable.{Set, Map}
import space.scown.regex.Nfa.string2Nfa
import scala.language.postfixOps

class DfaTest extends FunSpec {

  describe("Dfa") {
    describe("single matches") {
      it("should not match anything with the empty string") {
        val nfa: Nfa = ""
        val dfa = nfa.compile

        val matches = dfa.matches("")

        assert(matches === None)
      }

      it("should match single characters") {
        val nfa: Nfa = "0"

        val dfa = nfa.compile

        val matches = dfa.matches("0")

        assert(matches.get.matched === "0")
        assert(matches.get.next() === None)
      }

      it("should match concatenations") {
        val nfa: Nfa = "ab"

        val dfa = nfa.compile

        val matches = dfa.matches("ab")

        assert(matches.get.matched === "ab")
        assert(matches.get.next() === None)
      }

      it("should match unions") {
        val nfa: Nfa = "a"|"b"

        val dfa = nfa.compile

        assert(dfa.matches("a").get.matched === "a")
        assert(dfa.matches("b").get.matched === "b")
      }

      it("should match *") {
        val nfa: Nfa = "a"*

        val dfa = nfa.compile

        assert(dfa.matches("a").get.matched === "a")
        assert(dfa.matches("aa").get.matched === "aa")
      }

      it("should match a complex regex") {
        val nfa: Nfa = ("011"|"10")("1"*)|("00"|"11")("0"("1"*)|"")

        val dfa = nfa.compile

        assert(dfa.matches("00").get.matched === "00")
        assert(dfa.matches("11").get.matched === "11")
        assert(dfa.matches("011").get.matched === "011")
        assert(dfa.matches("01111").get.matched === "01111")
        assert(dfa.matches("01111").get.matched === "01111")
        assert(dfa.matches("10").get.matched === "10")
        assert(dfa.matches("101").get.matched === "101")
        assert(dfa.matches("1011").get.matched === "1011")
        assert(dfa.matches("0001").get.matched === "0001")
        assert(dfa.matches("1101").get.matched === "1101")
        assert(dfa.matches("000").get.matched === "000")
        assert(dfa.matches("00011").get.matched === "00011")
        assert(dfa.matches("110").get.matched === "110")
        assert(dfa.matches("11011").get.matched === "11011")
      }
    }

    describe("multiple matches") {
      it("should match single characters") {
        val nfa: Nfa = "0"

        val dfa = nfa.compile

        val matches = dfa.matches("00")

        assert(matches.get.matched === "0")
        assert(matches.get.next().get.matched === "0")
      }

      it("should match concatenations") {
        val nfa: Nfa = "ab"

        val dfa = nfa.compile

        val matches = dfa.matches("abab")

        assert(matches.get.matched === "ab")
        assert(matches.get.next().get.matched === "ab")
      }

      it("should match unions") {
        val nfa: Nfa = "a"|"b"

        val dfa = nfa.compile

        val matches = dfa.matches("ab")

        assert(matches.get.matched === "a")
        assert(matches.get.next().get.matched === "b")
      }

      it("should match *") {
        val nfa: Nfa = "a"*

        val dfa = nfa.compile

        val matches = dfa.matches("abaa")
        assert(matches.get.matched === "a")
        assert(matches.get.next().get.matched === "aa")
      }

      it("should match a complex regex") {
        val nfa: Nfa = ("011"|"10")("1"*)|("00"|"11")("0"("1"*)|"")

        val dfa = nfa.compile

        val matches = dfa.matches("00 11 011 01111 ")

        assert(dfa.matches("00").get.matched === "00")
        assert(dfa.matches("11").get.matched === "11")
        assert(dfa.matches("011").get.matched === "011")
        assert(dfa.matches("01111").get.matched === "01111")
        assert(dfa.matches("01111").get.matched === "01111")
        assert(dfa.matches("10").get.matched === "10")
        assert(dfa.matches("101").get.matched === "101")
        assert(dfa.matches("1011").get.matched === "1011")
        assert(dfa.matches("0001").get.matched === "0001")
        assert(dfa.matches("1101").get.matched === "1101")
        assert(dfa.matches("000").get.matched === "000")
        assert(dfa.matches("00011").get.matched === "00011")
        assert(dfa.matches("110").get.matched === "110")
        assert(dfa.matches("11011").get.matched === "11011")
      }
    }
  }

}
