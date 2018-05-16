package space.scown.regex

import org.scalatest.FunSpec
// exclude Predef to avoid conflicts
import scala.Predef.{augmentString => _}
import space.scown.regex.Nfa.string2Nfa
import scala.language.postfixOps

class DfaTest extends FunSpec {

  describe("Dfa") {
    describe("single matches") {
      it("should not match anything with the empty string") {
        val nfa: Nfa = ""
        val dfa = nfa.compile

        val matches = dfa.matches("")

        assert(matches === Stream())
      }

      it("should match single characters") {
        val nfa: Nfa = "0"

        val dfa = nfa.compile

        val matches = dfa.matches("0")

        assert(matches === "0" #:: Stream())
      }

      it("should match concatenations") {
        val nfa: Nfa = "ab"

        val dfa = nfa.compile

        val matches = dfa.matches("ab")

        assert(matches === "ab" #:: Stream())
      }

      it("should match unions") {
        val nfa: Nfa = "a"|"b"

        val dfa = nfa.compile

        assert(dfa.matches("a") === "a" #:: Stream())
        assert(dfa.matches("b") === "b" #:: Stream())
      }

      it("should match *") {
        val nfa: Nfa = "a"*

        val dfa = nfa.compile

        assert(dfa.matches("a") === "a" #:: Stream())
        assert(dfa.matches("aa") === "aa" #:: Stream())
      }

      it("should match a complex regex") {
        val nfa: Nfa = ("011"|"10")("1"*)|("00"|"11")("0"("1"*)|"")

        val dfa = nfa.compile

        assert(dfa.matches("00") === "00" #:: Stream())
        assert(dfa.matches("11") === "11" #:: Stream())
        assert(dfa.matches("011") === "011" #:: Stream())
        assert(dfa.matches("01111") === "01111" #:: Stream())
        assert(dfa.matches("10") === "10" #:: Stream())
        assert(dfa.matches("101") === "101" #:: Stream())
        assert(dfa.matches("1011") === "1011" #:: Stream())
        assert(dfa.matches("0001") === "0001" #:: Stream())
        assert(dfa.matches("1101") === "1101" #:: Stream())
        assert(dfa.matches("000") === "000" #:: Stream())
        assert(dfa.matches("00011") === "00011" #:: Stream())
        assert(dfa.matches("110") === "110" #:: Stream())
        assert(dfa.matches("11011") === "11011" #:: Stream())
      }
    }

    describe("multiple matches") {
      it("should match single characters") {
        val nfa: Nfa = "0"

        val dfa = nfa.compile

        val matches = dfa.matches("00")

        assert(matches === "0" #:: "0" #:: Stream())
      }

      it("should match concatenations") {
        val nfa: Nfa = "ab"

        val dfa = nfa.compile

        val matches = dfa.matches("abab")

        assert(matches === "ab" #:: "ab" #:: Stream())
      }

      it("should match unions") {
        val nfa: Nfa = "a"|"b"

        val dfa = nfa.compile

        val matches = dfa.matches("ab")

        assert(matches === "a" #:: "b" #:: Stream())
      }

      it("should match *") {
        val nfa: Nfa = "a"*

        val dfa = nfa.compile

        val matches = dfa.matches("abaa")
        assert(matches === "a" #:: "aa" #:: Stream())
      }

      it("should match a complex regex") {
        val nfa: Nfa = ("011"|"10")("1"*)|("00"|"11")("0"("1"*)|"")

        val dfa = nfa.compile

        val matches = dfa.matches("00 11 011 01111 10 101 1011 0001 1101 000 00011 110 11011")

        assert(matches === "00" #:: "11" #:: "011" #:: "01111" #:: "10" #:: "101" #:: "1011" #:: "0001" #:: "1101" #:: "000" #:: "00011" #:: "110" #:: "11011" #:: Stream())
      }
    }
  }

}
