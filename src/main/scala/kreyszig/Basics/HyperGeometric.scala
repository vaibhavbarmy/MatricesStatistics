package kreyszig.Basics

import MathematicalOperators.binomialCoefficient
import kreyszig.Basics.ProbabilityAndConditionalProbability.eventOutcomeProbabilityDensityFunction
import MeanVarianceMomentDiscreteFunc.{meanCalculator, varianceCalculator}

/**
 * Created with IntelliJ IDEA.
 * User: Vaibhav
 * Date: 5/6/15
 * Time: 9:06 PM
 * To change this template use File | Settings | File Templates.
 */
object HyperGeometric {

  /**
   * Method to return probability density function of HyperGeometric function
   * N,M,n,x order of function input
   *
   * @return
   */
  def pdfHyperGeometric:(Int, Int, Int, Int) => Double = (N:Int, M:Int, n: Int, x: Int) => (binomialCoefficient(M, x)*binomialCoefficient(N-M, n-x)).toDouble/binomialCoefficient(N,n).toDouble

  def meanHyperGeometric(N: Int, M: Int, n: Int):Double = {
    var eopdfs: List[eventOutcomeProbabilityDensityFunction[Double]] = List[eventOutcomeProbabilityDensityFunction[Double]]()
    for (x <- 0 until n+1) eopdfs = eopdfs :+ eventOutcomeProbabilityDensityFunction[Double](x, Option(x.toDouble), x => pdfHyperGeometric(N,M,n,x.toInt) )
    meanCalculator(eopdfs)
  }

  def varianceHyperGeometric(N: Int, M: Int, n: Int): Double = {
    var eopdfs: List[eventOutcomeProbabilityDensityFunction[Double]] = List[eventOutcomeProbabilityDensityFunction[Double]]()
    for (x <- 0 until n+1) eopdfs = eopdfs :+ eventOutcomeProbabilityDensityFunction[Double](x, Option(x.toDouble), x => pdfHyperGeometric(N,M,n,x.toInt) )
    varianceCalculator(eopdfs)
  }

  def main(args: Array[String]){
    println(pdfHyperGeometric(4,2,3,1))
    println(meanHyperGeometric(4,2,3))
    println(varianceHyperGeometric(4,2,3))
    println(binomialCoefficient(10,3))
  }
}
