# regex-engine

An implementation of a simple Regular Expression engine in Scala.

## Usage

```scala
// This is necessary to prevent conflicts
import scala.Predef.{augmentString => _}

import space.scown.regex.{Dfa, Nfa}
import space.scown.regex.Nfa.string2Nfa
import scala.language.postfixOps


val nfa = ("011"|"10")("1"*)|("00"|"11")("0"("1"*)|"")
val dfa = nfa.compile

val matches: Stream[String] = dfa.matches("0111")
```

## Is it any good?

No! In particular, the library has a limited feature set and has not been optimised for
performance. Use the Scala standard library instead.

## License

Copyright 2018 Alex Scown. Licensed under the MIT license (the LICENSE).