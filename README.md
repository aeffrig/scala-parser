psp-parser
==========

Originally a fork of [scala-parser](https://github.com/lihaoyi/scala-parser), which notes that

    scala-parser originally was a fork of https://github.com/rcano/sps,
    but has changed so much that it is completely unrecognizable.

...right back at you.

It parses all the scala code I have yet attempted - about 40K source files. Pointers to counterexamples greatly appreciated. You can parse all files under a given path with

    sbt 'run /path/to/files'

A source file is only considered a failure if scalac parses it successfully but this parse does not.

Note that I'm quite intentionally parsing a significant superset of existing scala code. A great deal of logic disappears from the parser, and one can easily enforce a (now modularly defined) set of constraints after the fact, and with far superior error messages.

Such parser uniformity opens the door to remove the arbitrarily determined ad hoc restrictions present in the scala language: we don't fail immediately on multiple type parameter lists, named and default type parameters, method definitions without parameter types, annotations and modifiers on any kind of definition, trait constructor parameters, and so on.
