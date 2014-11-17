package kreyszig.Basics

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 11/18/14
 * Time: 1:38 AM
 * To change this template use File | Settings | File Templates.
 */
import MathematicalOperators._

object MeanVarianceQuartile {

  /**
   * Method to compute a opration with the schema ( SomeOperator on (sigma or pie or anything on ( dataset and its derived parameter) ))
   *
   * @param constituentOperator
   * @param resultOperator
   * @param dataSet
   * @param initialValue
   * @return
   */
  def generalizedMethod(constituentOperator: (Double, Double) => Double ) ( resultOperator: Double => Double) ( dataSet: List[Double], initialValue : Double): Double = {
    def iterator(currentDataSet : List[Double],acc: Double): Double = {
      if (currentDataSet.nonEmpty) iterator(currentDataSet.tail, constituentOperator(acc, currentDataSet.head))
      else resultOperator(acc)
    }
    iterator(dataSet, initialValue)
  }

  def arithmeticMean(dataSet: List[Double]) = generalizedMethod((x, y) => x + y) ( x => x/dataSet.size) ( dataSet, 0.0)

  def geometricMean(dataSet: List[Double]) = generalizedMethod((x,y) => x*y ) ( x => nthRootAvgDamp(x, dataSet.size)) ( dataSet, 1.0)

  def harmonicMean(dataSet: List[Double]) = generalizedMethod((x,y) => (1/y) + x ) ( x => posAndNegPowerOperator(x, -1)*dataSet.size) ( dataSet, 0.0)

  def variance(dataSet: List[Double]) = {
    val mean = arithmeticMean(dataSet)
    generalizedMethod((x,y) => x + posAndNegPowerOperator((y - mean) , 2)) ( x => x/(dataSet.size)) (dataSet, 0.0)
  }

  def main(args: Array[String]){
    val dataSet: List[Double] = List(2.0,4.0,8.0)
    println(generalizedMethod((x, y) => x + y)( x => x/dataSet.size)( dataSet, 0.0))
    println(generalizedMethod((x,y) => x*y )( x => cubeRootAvgDamp(x))( dataSet, 1.0))
    //println(cubeRootAvgDamp(27.0))
    //println(positivePowerOperator(3.0, 2))
    println(posAndNegPowerOperator(3.0, -1))
    //println(nthRootAvgDamp(3.0, 2))
    println(arithmeticMean(dataSet))
    println(geometricMean(dataSet))
    println(harmonicMean(dataSet))
    println(variance(dataSet))
  }
}
