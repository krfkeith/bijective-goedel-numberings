package bijectiveGoedelNumberings

/**
 * Main entry point to this package:
 * runs the tests from TermTester
 */
object Main extends App {

  TermTester.test(3)

}

/**
 * Contains methods testing
 * the generalized Cantor bijection
 * between N and N^k
 */
object CantorTester extends Cantor {
  def test {
    println("testing")
    val xs: List[BigInt] = List(2, 0, 1, 2, 5, 6, 7, 0, 2, 1)
    println("xs=" + xs)
    val ys = list2set(xs)
    println("ys=" + ys)
    val zs = set2list(ys)
    println("zs=" + zs)
    println("ok=" + (xs == zs))
    println("binomial(123,45)=" + binomial(123, 54))
    val ns: List[BigInt] = List(1, 0, 0, 2, 2, 0, 2, 1, 6, 0, 0, 3)
    val n = fromCantorTuple(ns)
    println(34567890 + "=tupling=" + n)

    println("comb=" + toCombinadics(5, 72))

    val ms = toCantorTuple(ns.length, n)
    println(ns + "=untupling=" + ms)

  }
}

/**
 * Contains testing functions trying
 * out independently the two sides of the
 * bijection between terms and natural numbers.
 */
object TermTester extends TermEncoder {

  /**
   * builds a linear term of size n
   * e.g. for n=3 something like
   *
   * F3(v3,F2(v2,F1(v1,v0,F1),F2),F3)
   */
  def bigt(n: BigInt): Term = {
    if (0 == n) V(0)
    else {
      F(n,
        List(V(n), bigt(n - 1), F(n, Nil)))
    }
  }

  /**
   * builds a deep term of size O(2^n)
   * e.g. for n=3 something like
   *
   * F3(v3,F2(v2,F1(v1,v0,v0),F1(v1,v0,v0)),F2(v2,F1(v1,v0,v0),F1(v1,v0,v0)))
   */
  def bigtt(n: BigInt): Term = {
    if (0 == n) V(n)
    else {
      val b = bigtt(n - 1)
      F(n, List(V(n), b, b))
    }
  }

  /**
   * runs various tests on
   * individual and combined algorithms
   */
  def test(n: BigInt) {

    val t = bigt(n)

    println("t=" + t)
    /*
    val s = toSkel(t)
    println(s.length + ":s=" + s)
    val xs = toSyms(t)
    println(xs.length + ":tsyms=" + xs)
    */

    val tt = bigtt(n)
    println("tt=" + tt)

    /*
    val ss = toSkel(tt)
    println(ss.length + ":ss=" + ss)
    val ys = toSyms(tt)
    println(ys.length + ":ttsyms=" + ys)
    val pp = toPair(tt)
    val tt1 = fromPair(pp)
    println("pp=" + pp)
    println("tt1=" + tt1)
    */

    val r = toCode(t)
    println("r=" + r)
    val rr = toCode(tt)
    println("rr=" + rr)
    val t2 = fromCode(r)

    println("t2=" + t2)

    val tt2 = fromCode(rr)

    println("tt2=" + tt2)

    val b = BigInt(1234567890)
    val bt = fromCode(b)
    println("b=" + b)
    println("bt=" + bt)

    val b1 = BigInt("12345678900987654321")
    val bt1 = fromCode(b1)
    println("b1=" + b1)
    println("bt1=" + bt1)

    val b2 = b1 * b1 * b1
    val bt2 = fromCode(b2)
    println("b2=" + b2)
    println("bt2=" + bt2)
    println("b2=" + toCode(bt2))

    println("END")
  }

}