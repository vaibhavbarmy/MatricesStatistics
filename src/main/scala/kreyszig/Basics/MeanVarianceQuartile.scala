package kreyszig.Basics

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 11/18/14
 * Time: 1:38 AM
 * To change this template use File | Settings | File Templates.
 */
import MathematicalOperators._
import BaseAlgorithms._

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

  /**
   *
   * @param dataSet
   * @tparam N
   * @return
   */
  def quartile[N](sortFunc: Array[N] => Array[N])(additionFunc: (Double,Double) => Double, divideFunc: (Double,Double) => Double)(dataSetWhole: Array[N]):(N,N,N) = {
    val sortedDataSet:Array[N] = sortFunc(dataSetWhole)
    def medianCalc(dataSet: Array[N]):N={
      if(dataSet.length%2 == 0) divideFunc(additionFunc(parse[Double](dataSet(dataSet.length/2).toString).getOrElse(0.0),parse[Double](dataSet(dataSet.length/2 - 1).toString).getOrElse(0.0)),2.0).asInstanceOf[N]
      else  dataSet((dataSet.length/2) )
    }
    val median = medianCalc(dataSetWhole)
    val (lowerQuartile, upperQuartile):(N,N) = if (dataSetWhole.length%2 != 0) {
      val (belowMedian, aboveMedian) = sortedDataSet.splitAt(sortedDataSet.length/2)
      val trueAboveMedian = aboveMedian.splitAt(1)._2
      (medianCalc(belowMedian), medianCalc(trueAboveMedian))
    }
    else {
      val (belowMedian, aboveMedian) = sortedDataSet.splitAt(sortedDataSet.length/2)
      (medianCalc(belowMedian), medianCalc(aboveMedian))
    }
    (median,lowerQuartile,upperQuartile)
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

    val num = 2.0
    val power = .8
    import scala.math.pow
    var currentTime = System.currentTimeMillis()
    //println(positivePowerOperator(num, power))
    println(System.currentTimeMillis() - currentTime)

    currentTime = System.currentTimeMillis()
    println(pow(num, power))
    println(System.currentTimeMillis() - currentTime)
    val x = List(3,7,8,5,2,1,9,5,4,3)
    println(quickSort((x,y) => (x+y)/2 )((x:Int,y:Int) => x < y)(x.toArray).toList)
    println("test", quartile(quickSort((x,y) => (x+y)/2 )((x:Int,y:Int) => x < y) _)((x:Double,y:Double) => (x+y), (x:Double,y: Double) => x/y)(x.toArray))
  }
}
