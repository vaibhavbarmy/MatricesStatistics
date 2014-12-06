package kreyszig.Basics

import kreyszig.Basics.MathematicalOperators._

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
   * Method to merge sort a input list taking merge function as a parameter
   *
   * @param mergeFunc
   * @param inputList
   * @return
   */
  def mergeSort(mergeFunc: (List[Double], List[Double]) => List[Double])(inputList: (List[Double])): List[Double] = {

    val midPoint = inputList.length/2
    inputList match {
      case List() => inputList
      case List(x) => inputList
      case _ => {
        val (firstPart, secondPart) = inputList splitAt midPoint
        mergeFunc(mergeSort(mergeFunc)(firstPart), mergeSort(mergeFunc)(secondPart))
      }
    }
  }

  /**
   * Method to quick sort a Numeric array
   *
   * @param pivotFunc       Function to assume a pivot point
   * @param comparatorFunc  Funcations to compare two numeric points
   * @param array
   * @tparam N
   * @return
   */
  def quickSort[N](pivotFunc: (Int,Int) => Int)(comparatorFunc: (N,N) => Boolean)(array: Array[N]): Array[N] = {
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
    sort(0,array.length - 1)
    tmpArray
  }

  /**
   * Method to calculate median for a numeric array
   *
   * @param sortFunc
   * @param additionFunc
   * @param divideFunc
   * @param dataSet
   * @tparam N
   * @return
   */
  def medianCalc[N](sortFunc: Array[N] => Array[N])(additionFunc: (Double,Double) => Double, divideFunc: (Double,Double) => Double)(dataSet: Array[N]):N={
    val sortedDataSet = sortFunc(dataSet)
    if(sortedDataSet.length%2 == 0) divideFunc(additionFunc(parse[Double](sortedDataSet(sortedDataSet.length/2).toString).getOrElse(0.0),parse[Double](sortedDataSet(sortedDataSet.length/2 - 1).toString).getOrElse(0.0)),2.0).asInstanceOf[N]
    else  sortedDataSet((sortedDataSet.length/2))
  }

  /**
   * Method to get the union of seq of event Sets
   *
   * @param eventsList seq of event sets
   * @tparam T        Type of event values
   * @return
   */
  def union[T](eventsList: List[T]*): List[T] = {
    def iter[T](accList: List[T], remainderEventsList: Seq[List[T]]): List[T] = {
      if(remainderEventsList.isEmpty) accList
      else {
        val head:List[T] = remainderEventsList.head
        val tail:Seq[List[T]] = remainderEventsList.tail
        val union: List[T] = accList.union(head)
        val intersection: List[T] = accList.intersect(head)
        val properUnion = union.diff(intersection)
        iter[T](properUnion, tail)}
    }
    val seqOfEvents: Seq[List[T]] = eventsList.toSeq
    iter[T](List[T](), seqOfEvents)
  }

  /**
   * Method to get intersection of seq of event Lists
   *
   * @param eventsList seq of event Lists
   * @tparam T        type of event values
   * @return
   */
  def intersection[T](eventsList: List[T]*): List[T] = {
    def iter[T](accList: List[T], remainderEventsList: Seq[List[T]]): List[T] = {
      if(remainderEventsList.isEmpty) accList
      else if(accList.isEmpty) iter[T](remainderEventsList.head, remainderEventsList.tail)
      else iter[T](accList.intersect(remainderEventsList.head), remainderEventsList.tail)
    }
    val seqOfEvents: Seq[List[T]] = eventsList.toSeq
    iter[T](List[T](), seqOfEvents)
  }

  /**
   * Method to get compliment of a event List corresponding to sample space
   *
   * @param sampleSpace sample space for all the events
   * @param eventList    event List whose compliment is to get
   * @tparam T          Type of event value
   * @return
   */
  def compliment[T](sampleSpace: List[T], eventList: List[T]): List[T] = {
    sampleSpace.diff(eventList)
  }

  def main(args: Array[String]){
    println(mergeSort(mergePairMatch _)(List(2.4,1.0,8.5,9.3,2.7)))
    val x = List(3,7,8,5,2,1,9,5,4)
    //var y = new Array[Int](3)
    println(x.toArray)
    //y.update(0,5)
    //println(y(1))
    val y = x.toArray
    //println(y(1))
    val a = List(100,200,500,5,5)
    val z = List(5, 5, 7, 8, 9,3)
    println(z.toSet.toList)
    println("union", union[Int](z ,y.toList, a))
    println(quickSort((x,y) => (x+y)/2 )((x: Int,y: Int) => x < y)(x.toArray).toList)
    println(quickSort((x,y) => (x+y)/2 )((x: Int,y: Int) => x < y)(z.toArray).toList)
    println(medianCalc(quickSort((x,y) => (x+y)/2 )((x:Int,y:Int) => x < y) _)((x:Double,y:Double) => (x+y), (x:Double,y: Double) => x/y)(z.toArray))
  }

}
