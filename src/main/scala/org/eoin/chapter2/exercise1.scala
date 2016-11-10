package org.eoin.chapter2

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
