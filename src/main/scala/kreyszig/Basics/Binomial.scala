package kreyszig.Basics

import MathematicalOperators.binomialCoefficient
import scala.math.pow
import MeanVarianceMomentDiscreteFunc.{meanCalculator, varianceCalculator}
import kreyszig.Basics.ProbabilityAndConditionalProbability.eventOutcomeProbabilityDensityFunction

/**
 * Created with IntelliJ IDEA.
 * User: Vaibhav
 * Date: 5/6/15
 * Time: 11:25 AM
 * To change this template use File | Settings | File Templates.
 */
object Binomial {

  /**
   * Method to return the probability density function of a binomial distribution
   *
   * @return
   */
  def pdfBinomial: (Int, Int, Double)=>Double = (n:Int, x:Int, p:Double) => binomialCoefficient(n, x)*pow(p, x)*pow(1-p, n-x)

  /**
   * Method to return the mean of a binomial distribution i.e n*p
   *
   * @param n Total no of trials
   * @param p Probability of success of a trial
   * @return
   */
  def meanBinomial(n: Int, p: Double): Double = {
    var eopdfs: List[eventOutcomeProbabilityDensityFunction[Double]] = List[eventOutcomeProbabilityDensityFunction[Double]]()
    for (x <- 0 until n+1) eopdfs = eopdfs :+ eventOutcomeProbabilityDensityFunction[Double](x, Option(x.toDouble), x => pdfBinomial(n,x.toInt,p) )
    meanCalculator(eopdfs)
  }

  /**
   * Method to return the variance of a binomial distribution i.e n*p*q
   *
   * @param n Total no .of trials
   * @param p Probability of success of a trial
   * @return
   */
  def varianceBinomial(n: Int, p: Double): Double = {
    var eopdfs: List[eventOutcomeProbabilityDensityFunction[Double]] = List[eventOutcomeProbabilityDensityFunction[Double]]()
    for (x <- 0 until n+1) eopdfs = eopdfs :+ eventOutcomeProbabilityDensityFunction[Double](x, Option(x.toDouble), x => pdfBinomial(n,x.toInt,p) )
    varianceCalculator(eopdfs)
  }

  def main(args: Array[String]){
    println(pdfBinomial(4,3,0.5))
    println(meanBinomial(8,0.3))
    println(varianceBinomial(8, 0.5))
    //Exercise 24.7
    //2.
    println(1-pdfBinomial(20,0,0.05))

  }
}
