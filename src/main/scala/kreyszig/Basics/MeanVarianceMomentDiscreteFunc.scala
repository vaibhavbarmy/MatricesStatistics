package kreyszig.Basics

import kreyszig.Basics.ProbabilityAndConditionalProbability.{eventOutcomeProbability, eventOutcomeProbabilityDensityFunction, probabilityCalculator}
import scala.math.pow
/**
 * Created with IntelliJ IDEA.
 * User: Vaibhav
 * Date: 5/5/15
 * Time: 7:03 PM
 * To change this template use File | Settings | File Templates.
 */
object MeanVarianceMomentDiscreteFunc {

  /**
   * Method to calculate kth moment of a random variable X
   *
   * @param dataSet
   * @param k
   * @tparam T
   * @return
   */
  def kthMomentCalculator[T](dataSet: List[eventOutcomeProbabilityDensityFunction[T]])(k: Double): Double = {
    val eventOutcomeProbability: List[eventOutcomeProbability[T]] = probabilityCalculator[T](dataSet)
    val listOfKthExpectations: List[Double] = eventOutcomeProbability.map( eop => pow(eop.outcome.get.asInstanceOf[Double], k)*eop.probability.get)
    listOfKthExpectations.aggregate(0.0)(_ + _, _ + _)
  }

  /**
   * Method to calculate kth central moment of a random variable X with mean u
   *
   * @param dataSet
   * @param k
   * @tparam T
   * @return
   */
  def kthCentralMomentCalculator[T](dataSet: List[eventOutcomeProbabilityDensityFunction[T]])(k: Double): Double ={
    val mean: Double = meanCalculator[T](dataSet)
    val eventOutcomeProbability: List[eventOutcomeProbability[T]] = probabilityCalculator[T](dataSet)
    val listOfKthCentralMoment: List[Double] = eventOutcomeProbability.map( eop => pow((eop.outcome.get.asInstanceOf[Double] - mean), k)*eop.probability.get)
    listOfKthCentralMoment.aggregate(0.0)(_ + _, _ + _)
  }

  /**
   * Method to calculate mean of random variable X i.e first moment
   *
   * @param dataSet
   * @tparam T
   * @return
   */
  def meanCalculator[T](dataSet: List[eventOutcomeProbabilityDensityFunction[T]]) : Double = kthMomentCalculator[T](dataSet)(1.0)

  /**
   * Method to calculate variance of a random variable X i.e second central moment
   *
   * @param dataSet
   * @tparam T
   * @return
   */
  def varianceCalculator[T](dataSet: List[eventOutcomeProbabilityDensityFunction[T]]) : Double = kthCentralMomentCalculator[T](dataSet)(2.0)

  def main(args: Array[String]){
    val x = List[Int](1,2,5)
    println(x.aggregate(0)(_ + _, _ + _))
    var eopdfs: List[eventOutcomeProbabilityDensityFunction[Double]] = List[eventOutcomeProbabilityDensityFunction[Double]]()
    for (x <- 1 until 21) eopdfs = eopdfs :+ eventOutcomeProbabilityDensityFunction[Double](x, Option(x.toDouble*1.0), x => 1/20.0)
    //x.toDouble*1.0
    println(eopdfs)
    println(meanCalculator[Double](eopdfs))
    println(varianceCalculator[Double](eopdfs))
    println(kthMomentCalculator[Double](eopdfs)(2.0))
    println(kthCentralMomentCalculator[Double](eopdfs)(5.0))
  }
}
