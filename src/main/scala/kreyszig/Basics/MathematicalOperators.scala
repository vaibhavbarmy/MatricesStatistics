package kreyszig.Basics

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 11/18/14
 * Time: 2:34 AM
 * To change this template use File | Settings | File Templates.
 */
object MathematicalOperators {

  val tolerance = 0.0001

  /**
   * Method to take the absolute of a input
   * @param x the input double value
   * @return
   */
  def abs(x: Double) = if (x >= 0) x else -1*x

  def abs(x: Int): Int = if (x >= 0) x else -1*x

  def sqrtAvgDamp(x: Double) = fixedPoint(avgDamp(y => x/y), 1)

  def cubeRootAvgDamp(x: Double) = fixedPoint(avgDamp(y => x/(y*y)), 1)

  def nthRootAvgDamp(x: Double, n: Int) = fixedPoint(avgDamp(y => x/positivePowerOperator(y, n -1)), 1)
  /**
   *
   * @param f function operator
   * @param x value
   * @return
   */
  def avgDamp(f: Double => Double)(x: Double) = (x + f(x))/2

  /**
   *
   * @param value the next value which is to be compared to the initial guess
   * @param guess the initial guess
   * @return      Boolean
   */
  def isGoodEnough(value: Double, guess: Double) :Boolean = if (abs((value - guess)/guess)/guess < tolerance ) true else false

  /**
   *
   * @param f          the function to get the fixed point
   * @param firstGuess the initial guess estimate
   * @return           the value of fixed point
   */
  def fixedPoint(f: Double => Double, firstGuess: Double) = {
    def iterate(guess: Double): Double ={
      val newerGuess = f(guess)
      if(isGoodEnough(newerGuess, guess)) newerGuess else iterate(newerGuess)
    }
    iterate(firstGuess)
  }

  /**
   *
   * @param x     value whose power to be computed
   * @param power positive value of power
   * @return
   */
  def positivePowerOperator(x: Double, power: Int): Double = {
    def iterator(power: Int, acc : Double) : Double = {
      if(power == 0 ) acc else iterator(power - 1, acc * x)
    }
    iterator(power, 1)
  }

  /**
   *
   * @param x     value whose power to be computed
   * @param power positive or negative power
   * @return
   */
  def posAndNegPowerOperator(x: Double, power: Int): Double = {
    if (power < -1) fixedPoint(avgDamp(y => x/(positivePowerOperator(y, (abs(power) - 1)))), 1)
    else if (power == -1) 1/x
    else positivePowerOperator(x, power)
  }

  /**
   * Method to calculate gcd
   * @param x The first parameter for gcd
   * @param y The decond parameter for gcd
   * @return The gcd for the inputs
   */
  def gcd(x: Int , y: Int): Int = if (y == 0) x else gcd(y, x % y)

  /**A very basic and incorrect method for a double type power operator
   *
   * @param x     value whose power to be computed
   * @param power positive value of power
   * @return
   */
  def positivePowerOperator(x: Double, power: Double): Double = {
    def getWholeAndFractionDouble(x: Double): (Int, Int) = {
      val exponent = x.toInt
      val mantissa = (x - exponent).toFloat
      (exponent, Integer.valueOf(mantissa.toString.split("\\.")(1)))
    }
    val (whole, frac) = getWholeAndFractionDouble(power)
    val multiplier = positivePowerOperator(10,frac.toString.size).toInt

    val wholeNumer = (power * multiplier).toInt
    val gcdValue = gcd(wholeNumer, multiplier)

    posAndNegPowerOperator(posAndNegPowerOperator(x, wholeNumer/gcdValue), -1*multiplier/gcdValue)
  }
  case class ParseOp[T](op: String => T)
  implicit val popDouble = ParseOp[Double](_.toDouble)
  implicit val popInt = ParseOp[Int](_.toInt)
  def parse[T: ParseOp](s: String) = try { Some(implicitly[ParseOp[T]].op(s)) }
  catch {case _ => None}

}
