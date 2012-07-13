package bijectiveGoedelNumberings

/**
 * Bijective Goedel numberings (ranking / unranking functions)
 * for Term Algebras with a finite signature and a finite number
 * of variables. Models things like circuits made
 * of a fixed library of gates and a fixed number of input
 * wires.
 *
 * While the bitsize of the natural numbers associated
 * to some terms might exceed the bitsize of the term
 * representation, terms associated to a small numbers
 * a guaranteed to be also small.
 *
 * These algorithms are likely to be useful for
 * random generation of terms - of use in circuit
 * synthesis or genetic programming - but they are
 * unlikely to be useful as a practical serialization
 * mechanism.
 */
sealed trait FixedSignatureTerm[A, B]

/**
 * Variables of polymorphic type A - usually given
 * as a finite list.
 */
case class FVar[A, B](a: A) extends FixedSignatureTerm[A, B] {
  override def toString = a.toString
}

/**
 * Constant symbols of polymorphic type B - usually given as
 * a finite list.
 */
case class FConst[A, B](b: B) extends FixedSignatureTerm[A, B] {
  override def toString = b.toString
}

/**
 * Function symbols of polymorphic type B - usually given
 * as a finite list (together with their arities).
 */
case class FFun[A, B](f: B, xs: List[FixedSignatureTerm[A, B]])
    extends FixedSignatureTerm[A, B] {
  override def toString = {
    val s = new StringBuilder()
    s.append(f)
    s.append("(")
    xs match {
      case Nil => //
      case y :: ys => {
        s.append(y)
        ys.foreach(x => {
          s.append(",")
          s.append(x)
        })
      }
    }
    s.append(")")
    s.toString
  }
}

/**
 * Algorithms computing a bijection between Natural Numbers
 * represented as BigInts and terms taken from an algebra
 * with finite signature - parameterized by the type of
 * the variables (A) and the type of the function+constant
 * symbols (B).
 */
class FGoedelNums[A, B](
  vars: Array[A], consts: Array[B], funs: Array[(B, BigInt)])
    extends Catalan with Cantor {
  val lv = vars.length
  val lc = consts.length
  val lf = funs.length
  val lvc = lv + lc

  /**
   * Builds the unique term associated to
   * natural number n, using symbols from the arrays
   * "consts" and "funs" and variables from the
   * array "vars".
   *
   * The algorithm works by associating
   * small numbers to variable and constant symbols
   * and then it uses the generalized Cantor bijection
   * toCantorTuple to split larger numbers into
   * sequences matching the arities of the
   * function symbols, from which it creates
   * subterms recursively.
   */
  def nat2term(n: BigInt): FixedSignatureTerm[A, B] = {
    if (0 <= n && n < lv) {
      val i: Int = n.intValue // index in Array of variables
      FVar(vars(i))
    } else if (lv <= n && n < lvc) {
      val i = (n - lv).intValue //index in the array of Constants
      FConst(consts(i))
    } else { // when lvc <= n 
      val n1 = (1 + n - lvc)
      // extracts excactly one (bijecting base lf)
      // digit from n to use it as an index that
      // selects a function symbol
      val (d, m) = get_bdigit(lf, n1)
      val i = d.intValue // index in the array of functions symbols
      val (f, k) = funs(i)
      // splits m into a list of natrual numbers ns
      val ns = toCantorTuple(k, m)
      // applies the same recursively on the list ns
      // using "map" and the builds a term based on
      // function symbol "f"
      new FFun(f, ns map nat2term)
    }
  }

  /**
   * Returns the unique natural number (represented as a BigInt)
   * associated to a term t.
   */
  def term2nat(t: FixedSignatureTerm[A, B]): BigInt = t match {
    case FVar(x)   => vars.indexOf(x) // decodes x as a variable
    case FConst(c) => lv + consts.indexOf(c) // decodes c as a constant
    case FFun(f, xs) => {
      val k = xs.length // decodes (f,k) to function symbol d
      val d = funs.indexOf((f, k))
      // recurses over subterms
      val ns = xs map term2nat
      // aggregates codes of subterms
      val m = fromCantorTuple(ns)
      // aggregates d (seen as a bijectiv base lf digit)
      // and code of obtained from subterms m
      val n = put_bdigit(lf, d, m)
      // shifts values to appropriate range
      n + lvc - 1
    }
  }

  /**
   * extract one bijective base-b digit from
   * natiral number n
   */
  def get_bdigit(b: BigInt, n: BigInt) = {
    val (q, d) = (n / b, n % b)

    if (d == 0) (b - 1, q - 1)
    else (d - 1, q)
  }

  /**
   * integrates bijective base-b digit d into natural
   * number m
   */
  def put_bdigit(b: BigInt, d: BigInt, m: BigInt) = 1 + d + (b * m)

}

