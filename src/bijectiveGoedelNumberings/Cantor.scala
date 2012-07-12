package bijectiveGoedelNumberings
import scala.collection.mutable._

/**
 * Implements fast algorithms for the bijection between natural numbers and
 * tuples of natural numbers using a generalization of Cantor's pairing bijection -
 * along the lines of
 *
 * @INPROCEEDINGS{iclp12,
 * author = {Tarau, Paul},
 * title = {{Deriving a Fast Inverse of the Generalized Cantor N-tupling Bijection}},
 * booktitle = {{28-th International Conference on Logic Programming -
 * Technical Communications (ICLP'12)}},
 * editor = {Dovier, Agostino and Costa, Vitor Santos},
 * year = {2012},
 * address = {Budapest, Hungary},
 * month = sep
 * }
 */
trait Cantor extends Catalan {
  def list2set(xs: List[BigInt]): List[BigInt] = {
    var s: BigInt = -1
    val i = xs.iterator
    val ys = new ArrayBuffer[BigInt]
    while (i.hasNext) {
      val y = i.next
      val z = s + y + 1
      ys += z
      s = z
    }
    ys.toList
  }
  /**
   * Bijection from finite sets of natural numbers
   * (canonically represented as non-decreasing sequences)
   * to finite sequences of natural numbers
   */
  def set2list(xs: List[BigInt]): List[BigInt] = {
    var x: BigInt = -1
    val i = xs.iterator
    val ys = new ArrayBuffer[BigInt]
    while (i.hasNext) {
      val y = i.next
      val z = y - x - 1
      ys += z
      x = y
    }
    ys.toList
  }

  /**
   * Computes the natural number uniquely associated
   * to the sequence of natural numbers ns, using
   * Cantor's generalized bijection formula
   */
  def fromCantorTuple(ns: List[BigInt]): BigInt = {
    val xs = list2set(ns).toArray[BigInt]
    val l = xs.length
    var s = BigInt(0)

    for (i <- 0 until l) {
      val b = binomial(xs(i), i + 1)
      s += b
    }

    s
  }

  /**
   *  efficently finds the smallest m such that (m choose k > n) using
   *  binary search
   */
  def firstKBinomialLargerThan(k: BigInt, n: BigInt): BigInt = {
      def bsearch(from: BigInt, to: BigInt): BigInt = {
        if (from == to) from
        else {
          val mid = (from + to) >> 1
          if (binomial(mid, k) > n) bsearch(from, mid)
          else bsearch(mid + 1, to)
        }
      }

    bsearch(k - 1, n + k)
  }

  /**
   * Converts n to its "combinadic" representation
   * (see Knuth's AOCP fascicle for definitions)
   */
  def toCombinadics(k: BigInt, n: BigInt): List[BigInt] = {
    if (k == 0) List() else {
      val m = firstKBinomialLargerThan(k, n) - 1
      val newN = n - binomial(m, k)
      m :: toCombinadics(k - 1, newN)
    }
  }

  /**
   * Computes a sequence of k integers uniquely associated
   * to n using Cantor's generalized bijection.
   *
   * This is an effciently computed inverse of
   * fromCantorTuple - the key contribution
   * of Paul Tarau's iclp12 paper
   */
  def toCantorTuple(k: BigInt, n: BigInt): List[BigInt] = {

    val xs: List[BigInt] = toCombinadics(k, n)
    val rs = xs.reverse
    val ns = set2list(rs)
    ns
  }
}

