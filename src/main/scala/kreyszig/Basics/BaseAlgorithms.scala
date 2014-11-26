package kreyszig.Basics

/**
 * Created with IntelliJ IDEA.
 * User: vaibh_000
 * Date: 11/27/14
 * Time: 1:17 AM
 * To change this template use File | Settings | File Templates.
 */
object BaseAlgorithms {

  /**
   * Method to merge two list in pair case matching
   *
   * @param firstList
   * @param secondList
   * @return
   */
  def mergePairMatch(firstList: List[Double], secondList: List[Double]): List[Double] = (firstList, secondList) match {
    case (Nil, y) => y
    case (x, Nil) => x
    case (x :: xs, y :: ys) => {
      if (x > y) x :: mergePairMatch(xs, secondList)
      else y :: mergePairMatch(ys, firstList)
    }
  }

  /**
   * Method to merge and sort a list in descending order
   *
   * @param inputList
   * @return
   */
  def mergeSort(inputList: (List[Double]), mergeFunc: (List[Double], List[Double]) => List[Double]): List[Double] = {

    val midPoint = inputList.length/2
    inputList match {
      case List() => inputList
      case List(x) => inputList
      case _ => {
        val (firstPart, secondPart) = inputList splitAt midPoint
        mergeFunc(mergeSort(firstPart, mergeFunc), mergeSort(secondPart, mergeFunc))
      }
    }
  }

  def quickSort[N](array: Array[N])(pivotFunc: (Int,Int) => Int)(comparatorFunc: (N,N) => Boolean): Array[N] = {
    var tmpArray = array
    def sort[N](left: Int, right: Int){
      def swap(first: Int, second: Int) = {
        val tmpFirst = tmpArray(first)
        tmpArray.update(first, tmpArray(second))
        tmpArray.update(second, tmpFirst)
      }
      def partitionAndArrayUpdate(left: Int, right: Int): Int = {
        val pivotIndex = pivotFunc(left, right)
        val pivotValue = tmpArray(pivotIndex)
        swap(pivotIndex, right)
        var storeIndex = left
        for (i <- left until right ){
          if (comparatorFunc(tmpArray(i), pivotValue)){
            swap(i, storeIndex)
            storeIndex = storeIndex + 1
          }
        }
        swap(storeIndex, right)
        storeIndex
      }
      if (left < right) {
        val partitionIndex = partitionAndArrayUpdate(left, right)
        sort(left, partitionIndex - 1)
        sort(partitionIndex + 1, right)
      }
    }
    sort(0,8)
    tmpArray
  }

  def test(x: Int){
    x+1
  }


  def main(args: Array[String]){
    println(mergeSort(List(2.4,1.0,8.5,9.3,2.7), mergePairMatch _))
    val x = List(3,7,8,5,2,1,9,5,4)
    //var y = new Array[Int](3)
    println(x.toArray)
    //y.update(0,5)
    //println(y(1))
    val y = x.toArray
    //println(y(1))
    println(quickSort(x.toArray)((x,y) => (x+y)/2 )((x,y) => x < y).toList)
  }

}
