package bijectiveGoedelNumberings
import scala.collection.mutable._

/**
 * Implements bijective Goedel numberings
 * (ranking/unranking algorithms) for a term
 * algebra with an infinite supply of
 * function symbols and variables represented
 * as the Term data type
 *
 * a Term is:
 *   - a variable labeled with a natural number
 *   - a function symbol labeled with a natural number
 *       applied to a finite sequence of Terms
 *
 * note that this provides a bijection between the set
 * of _syntactically valid_ terms and natural numbers
 * in contrast with traditional Goedel numberings which
 *   - are not surjective
 *   - also associate ranks to syntactically invalid terms
 *
 * 1) the bijection ensures that the bitsizes of terms and
 * their Goedel Numbers are proportional - this follows
 * from the the use of a bijection between
 * the Catalan family of balanced parenthesis expressions
 * and "small" natural numbers in combination with the
 * polynomial encodings provided by the
 * generalized Cantor tupling bijection
 *
 * 2) in combination with a symbol table for function symbols
 * and heap position of variables used as labels, this can
 * provide a possibly practical serialization algorithm
 * for data types that can be seen as term algebras - including
 * XML files and object graphs
 *
 * 3) Term algebras can be seen as a generic syntax for well-formed
 * expressions in languages like predicate or lambda calculus as well
 * as a generic syntax for "proof-terms" in proof assistants like Coq.
 * Equivalently, they can be seen as programs expressed as syntax trees.
 * The encodings factor in the most obvious "decidable" properties
 * of our formal language objects (through an unambiguous CF-grammar).
 * By restricting Goedel numberings to only well formed terms
 * and ensuring that they are bijective, "no bit is lost"
 * through the mapping to natural numbers e.g. optimal
 * information theoretical succinctness is achieved.
 *
 * Note that "well-formedness" can safely be assumed as decidable and
 * covered by unambiguous context free grammars in today's formal languages.
 */
sealed trait Term {
  def toSkel(s: ArrayBuffer[Int])
  def toSyms(s: ArrayBuffer[BigInt])
}

/**
 * Variables labeled with natural numbers
 */
case class V(i: BigInt) extends Term {
  /**
   * produces a String representation of the name of
   * this variable
   */
  override def toString = "v" + i

  /**
   * Adds to the balanced parenthesis buffer
   * the "leaf object" marking the location of
   * this variable as a left+right perentehsis
   */
  override def toSkel(s: ArrayBuffer[Int]) {
    s.append(0)
    s.append(1)
  }

  /**
   * Adds to the buffer an encoding of this
   * variable as an even natural number
   */
  override def toSyms(s: ArrayBuffer[BigInt]) {
    s.append(2 * this.i)
  }
}

/**
 * Function terms labeled by a natural number f
 * and containing a sequence of subterms ts
 */
case class F(f: BigInt, ts: List[Term]) extends Term {
  override def toString = {
    val s = new StringBuilder()
    s.append("F")
    s.append(f.toString)
    if (!ts.isEmpty) {
      s.append("(")
      s.append(ts.head)
      for (t <- ts.tail) {
        s.append(",")
        s.append(t.toString)
      }
      s.append(")")
    }
    s.toString
  }

  /**
   * Adds to the balanced parenthesis buffer
   * a representation of this function and
   * its subterms
   */
  override def toSkel(s: ArrayBuffer[Int]) {
    s.append(0)
    for (t <- ts) {
      t.toSkel(s)
    }
    s.append(1)
  }

  /**
   * Adds to the buffer the labels of its function
   * symbol and the function and variable labels
   * of its subterms, recursively.
   */
  override def toSyms(s: ArrayBuffer[BigInt]) {
    if (ts.isEmpty) s.append(2 * this.f + 1)
    else {
      s.append(this.f)
      for (t <- ts) t.toSyms(s)
    }
  }
}

/**
 * Provides the encoding/decoding algorithms
 * from Terms in a term algebra to natural
 * numbers (represented as BigInt objects).
 *
 * First, it extracts a "Catalan Skeleton"
 * basicaly the sequence of "parentheses"
 * one can see in the string representation if
 * a term.
 *
 * Next, it extracts symbol and variable labels
 * from a term.
 *
 * Then it encodes the Catalan Skeleton as
 * a natural number, using the algorithms
 * in trait Catalan and
 *
 * it encodes the symbol and variable labels
 * using the genralized Cantor bijection
 * (the only known polynomial algorithm for the task).
 *
 * Finally it pairs together the two encodings
 * using the Cantor pairing function into
 * a natural number uniquely associated to the term.
 *
 * The decoding from a natural number to a term
 * proceeds by inverting each of these steps.
 *
 * The critical new algorithms used in the process are:
 *
 * - a fast inverse of the genralized Cantor bijection
 *   between N and N^K (see trait Cantor)
 *
 * - the joint "parsing" of a balanced parenthesis
 *   sequence and its associated symbol list into
 *   a unique term
 *
 * - the use of the (polynomial) Cantor bijection
 *   ensuring that
 *     -- a number of bitsize b is mapped
 *        to a term of bitsize O(b)
 *     -- a term (represented using b bits) is mapped
 *        to a natural number of bitsize O(b)
 *
 * The reason why this algorithm is unique is that
 * it ensures that:
 *
 * - only syntactically valid terms are encoded
 * - the size of the encoding is proportional
 *   with the size of the human readable representation
 *   (actually significantly smaller when written in base 10)
 * - every natural number decodes to a syntactically valid term
 */
trait TermEncoder extends Catalan with Cantor {
  type Pars = List[Int]
  type Syms = List[BigInt]

  /**
   * Extracts the "Catalan Skeleton" of term t,
   * a balanced parenthesis representation of
   * the syntactic structure of the term.
   */
  private def toSkel(t: Term): Pars = {
    val s = new ArrayBuffer[Int]()
    t.toSkel(s)
    s.toList
  }

  /**
   * Extracts the sequence of symbol labels
   * for variables and function symbols occurring
   * in term t
   */
  private def toSyms(t: Term): Syms = {
    val s = new ArrayBuffer[BigInt]()
    t.toSyms(s)
    s.toList
  }

  /**
   * Pairs together the structural and content information
   * of a term (Catalan Skeleton + Symbols)
   */
  def toPair(t: Term) = (toSkel(t), toSyms(t))

  /**
   * inverts the toPair operation - recovers a term
   * from its Catalan Skeleton + Symbols
   */
  def fromPair(pair: (Pars, Syms)) = {
      def parse_expr(ps: Pars, xs: Syms) = ps match {
        case 0 :: 1 :: cs => {
          val x = xs.head
          if ((x & 1) == 0) (V(x >> 1), cs, xs.tail)
          else (F(x >> 1, Nil), cs, xs.tail)
        }

        case 0 :: cs => {
          val (ts, cs1, xs1) = parse_list(cs, xs.tail)
          (F(xs.head, ts), cs1, xs1)
        }
        case x => sys.error("parse_expr: unparsable=" + x)
      }

      def parse_list(rcs: Pars, xs: Syms): (List[Term], Pars, Syms) =
        rcs match {
          case 1 :: cs => (Nil, cs, xs)
          case c :: cs => {
            val (t, cs1, xs1) = parse_expr(c :: cs, xs)
            val (ts, cs2, xs2) = parse_list(cs1, xs1)
            (t :: ts, cs2, xs2)
          }
          case x => sys.error("parse_list: unparsable=" + x)
        }

    parse_expr(pair._1, pair._2) match {
      case (t, Nil, Nil) => t
      case x             => sys.error("fromPair: unparsable=" + x)
    }
  }

  /**
   * Computes the number of "tree nodes" (when seeing a
   * list of balanced parenthesis as a tree). This turns
   * out to be exactly the number of left parenthesis - i.e.
   * half of the (even) length of the list ps.
   */
  private def treecount(ps: Pars): BigInt = ps.length >> 1

  /**
   * Encodes a term as a unique natural number
   * by first computing the pair reprsenting it
   * and then encoding it using the rank associated
   * to a parenthesis expression and the code
   * provide by the generalized Cantor bijection
   */
  def toCode(t: Term): BigInt = {
    val (ps, xs) = toPair(t)
    val r = rank(ps)
    val x = fromCantorTuple(xs)
    fromCantorTuple(List(r, x))
  }

  /**
   * Decodes a natural number to a unique
   * term by decoding a Catalan Skeleton
   * and a matching Symbol sequence from
   * which it parses the term using the
   * fromPair method.
   */
  def fromCode(n: BigInt): Term = {
    val List(r, x) = toCantorTuple(2, n)
    val ps = unrank(r)
    val xs = toCantorTuple(treecount(ps), x)
    fromPair(ps, xs)
  }
}

