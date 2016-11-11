package org.eoin

import scala.annotation.tailrec


object exercise1  {

  def fib(n: Int): Int = {
    require(n > 0 && n < Integer.MAX_VALUE)

    @tailrec def doFib (i: Int, nTh: Int, nPlusOneTh: Int): Int = {
      if (i == n) nTh
      else doFib(i + 1, nPlusOneTh, nTh + nPlusOneTh)
    }

    doFib(1,1,2)
  }
}


object exercise2 {

   def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
     //logger.debug(as.mkString(","))
     //logger.info(gt.getClass)
     //logger.info(as.length)
     if (as.length < 2 ) {
       true // 0 or 1 elements => already sorted
     } else {
       val comparePairs = as.sliding(2) find { pair: Array[A] => gt(pair(1), pair(0)) }
       comparePairs.isEmpty // if we didn't find one, then the whole array is sorted
     }
   }

}
