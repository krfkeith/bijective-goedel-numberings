A Scala package providing bijective Goedel numberings (ranking/unranking bijections to N) for members of a Term Algebra with an infinite supply of variables and functions symbols.

The algorithms, using a generalized Cantor bijection between N and N^k (known to be polynomial in size of the representations) ensure that:

- a unique syntactically valid term is associated to each natural number

- a unique natural number is associated to each syntactically valid term

- either way, the bitsize of the representation of the output is proportional (up to a small constant) to the bitsize of the representation of the output

For instance, a terms like:

F3(v3,F2(v2,F1(v1,v0,F1),F2),F3)
F3(v3,F2(v2,F1(v1,v0,v0),F1(v1,v0,v0)),F2(v2,F1(v1,v0,v0),F1(v1,v0,v0)))

are uniquely associated to a Goedel numbers like

1166589096937670191 and
781830310066286008864372141041

of comparable representation size.

Type "gcompile" to compile the files using the "scalac" compiler (available from typesafe.com or scala-lang.org).
That should create directory bin where class files are placed.

Type "grun" to run the tests in file Main.

To try out other examples edit Main.scala, recompile with "gcompile" and test with "grun".

Directory "doc" contains the scaladoc generated documentation - but you will need to
take a look at the source in directory "src" for any meaningful details on various
algorithms.

The code has been tested on a Mac with OS X Lion using scala version 2.9.2.

Enjoy,

Paul Tarau
http://www.cse.unt.edu/~tarau/

CHANGES:

The file FixedSignatureTerm.scala has been added. It contains algorithms for
encoding terms algebras of fixed signature. Main.scala has been updated 
with additional tests for these encodings.
