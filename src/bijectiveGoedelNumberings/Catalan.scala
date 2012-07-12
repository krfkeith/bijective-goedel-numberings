package bijectiveGoedelNumberings

import scala.collection.mutable._

/**
 * ranks and unranks balanced parenthesis strings
 * following algorithm form "Combinatorial Algorithms: ..."
 * by Kreher and Stinson
 *
 * it provides as public methods:
 *
 * binomial, rank, unrank, and showpars
 *
 */
trait Catalan {

  /**
   * Computes the binomial "n choose k"
   */
  def binomial(n: BigInt, r0: BigInt): BigInt = {
    var r = r0
    if ((r < 0) || (r > n)) 0
    else {
      // tests if it is easier to compute it for n-r
      if (2 * r > n) r = n - r
      var i = BigInt(0)
      var b = BigInt(1)
      if (r > 0)
        while (i < r) {
          b = b * (n - i) / (i + 1)
          i += 1
        }
      b
    }
  }

  /**
   * auxiliary function computing the difference of 2 binomials
   */
  private def m(n: BigInt, x: BigInt, y: BigInt) = {
    val n1 = 2 * n - x
    val r1 = n - (x + y) / 2
    binomial(n1, r1) - binomial(n1, r1 - 1)
  }

  /**
   * Computes rank for fixed size array containing
   * parenthesis expression using binary search
   */
  private def localRank(n: BigInt, a: Array[Int]): BigInt = {
    var x = 1
    var y = 0
    var lo = BigInt(0)
    var hi = m(n, 0, 0) - BigInt(1)
    while (x < 2 * n) {
      if (0 == a(x)) {
        hi -= m(n, x, y - 1)
        y += 1
      } else {
        lo += m(n, x, y + 1)
        y -= 1
      }
      x += 1
    }
    lo
  }

  /**
   * Computes natural number uniquely associated
   * to given balanced parentehsis string xs
   */
  def rank(xs: List[Int]): BigInt = {
    val l = xs.length - 2
    val a = xs.toArray[Int]
    var i = l / 2
    val n = localRank(i, a)
    var s = BigInt(0)
    i -= 1
    while (i >= 0) {
      var c = cat(i)
      s += c
      i -= 1
    }
    s + n
  }

  /**
   * Builds parenthesis string associated to n
   */
  private def localUnrank(n: BigInt, r: BigInt, a: Array[Int]) {
    var y = 0
    var lo = BigInt(0)
    var hi = m(n, 0, 0) - BigInt(1)
    var x = 1
    while (x <= 2 * n) {
      val k = m(n, x, y + 1)
      if (r < lo + k) {
        hi = lo + k - 1
        y += 1
        a(x) = 0
      } else {
        lo += k
        y -= 1
        a(x) = 1
      }
      x += 1
    }

  }

  /**
   * Computes Catalan number associated to n
   */
  private def cat(n: BigInt): BigInt = {
    if (n == 0) 1
    else
      2 * (2 * n - 1) * cat(n - 1) / (n + 1)
  }

  /**
   * Computes balanced parenthesis expression uniquely
   * associated to natural number r
   */
  def unrank(r: BigInt): List[Int] = {
    var s = BigInt(0)
    var i = 0

    var more = true
    while (more) {
      var c = cat(i)
      if (s + c <= r) {
        s += c
        i += 1
      } else
        more = false
    }
    val lr = r - s
    val l = 2 * i + 1
    val a = new Array[Int](l)
    localUnrank(i, lr, a)
    0 :: (a.toList.tail ++ List(1))
  }

  /*
   * Visualizes xs as parenthesis string
   */
  def showpars(xs: List[Int]) = {
    val s = new StringBuilder()
    for (x <- xs) {
      if (x == 0) s += '('
      else if (x == 1) s += ')'
      else s += '*'
    }
    s.toString
  }
}

