object Learn {

  class Rational(x: Int, y: Int) {
    def this(x: Int) = this(x, 1)
    require(y != 0, "rational denom can NOT be zero")
    private def gcd(a: Int, b: Int): Int = {
      if (b == 0) a else gcd(b, a % b)
    }
    private val g = gcd(x, y)
    val numer = x / g
    val denom = y / g
    def add(r: Rational): Rational = {
      new Rational(
        numer * r.denom + r.numer * denom,
        denom * r.denom)
    }
    def neg: Rational = {
      new Rational(-numer, denom)
    }
    def sub(r: Rational) = {
      add(r.neg)
    }
    def less(r: Rational) = {
      numer * r.denom < r.numer * denom
    }
    def max(r: Rational) = {
      if (this.less(r))
        r
      else
        this
    }
    def +(r:Rational) : Rational = {
      this.add(r)
    }
    def unary_- :Rational = neg
    override def toString = numer + "/" + denom
  }
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  x.add(y)
  x add y
  x + y
  - y
}

