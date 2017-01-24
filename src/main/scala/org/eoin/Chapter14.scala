package org.eoin

import org.eoin.Chapter14.STHashMap

import scala.language.higherKinds
import scala.reflect.ClassTag

/**
  * Created by eoin.parker on 1/10/17.
  */
object Chapter14 {

  sealed trait ST[S,A] { self =>
    protected def run(s: S): (A,S)
    def map[B](f: A => B): ST[S,B] = new ST[S,B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }
    }
    def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    }
  }

  trait RunnableST[A] {
    def apply[S]: ST[S,A]
  }

  object ST {
    def apply[S,A](a: => A) = {
      lazy val memo = a
      new ST[S,A] {
        def run(s: S) = (memo, s)
      }
    }

    def runST[S,A](st: RunnableST[A]) : A = {
      val stNullA = st.apply[Null]
      val (a,s) = stNullA.run(null)
      a
    }
  }

  sealed trait STRef[S,A] {
    protected var cell: A
    def read: ST[S,A] = ST(cell)
    def write(a: => A): ST[S,Unit] = new ST[S,Unit] {
      def run(s: S) = {
        cell = a
        ((), s)
  } }
  }
  object STRef {
    def apply[S,A](a: A):  ST[S, STRef[S,A]]  = ST(new STRef[S,A] { var cell = a })
  }


  sealed abstract class STArray[S,A](implicit manifest: ClassTag[A]) {
    protected def value: Array[A]
    def size: ST[S,Int] = ST(value.length)
    def write(i: Int, a: A):ST[S,Unit] = new ST[S,Unit] {
      override protected def run(s: S): (Unit, S) = {
        value(i) = a
        ((), s)
      }
    }

    def read(i: Int): ST[S,A] = ST (value(i))
    def freeze: ST[S,List[A]] = ST (value.toList)

     def fill(xs: Map[Int,A]): ST[S,Unit] = {
      val initialEmptyAction:ST[S,Unit] = ST[S,Unit]()
      xs.foldRight(initialEmptyAction){ (kv,st) =>
        val (k:Int,v:A) = kv
        st.flatMap[Unit] { _ => write(k, v) }
      }
    }
    def swap(i: Int, j: Int): ST[S, Unit] = for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()

 }

  object STArray {
    def apply[S,A:ClassTag](sz: Int, v: A): STArray[S,A] = new STArray[S,A] {
        override protected def value: Array[A] = Array.fill(sz)(v)
    }

    def fromList[S,A:ClassTag](xs: List[A]): ST[S, STArray[S,A]] = ST {
      new STArray[S,A] {
        override lazy val value: Array[A] = xs.toArray
      }
    }

//    def partition[S](arr: STArray[S,Int], l: Int, r: Int, pivot: Int): ST[S,Int] = for {
//      pivotVal <- arr.read(pivot)
//      _ <- arr.swap(pivot, r)
//      j <- STRef(l)
//      emptyAction = ST[S,Unit]()
//      _ <- (l until r).foldLeft( emptyAction )((actions, i) => for {
//        _ <- actions
//        vi <- arr.read(i)
//        _  <- if (vi < pivotVal) (for {
//          vj <- j.read
//          _  <- arr.swap(i, vj)
//          _  <- j.write(vj + 1)
//        } yield ()) else emptyAction
//      } yield ())
//      x <- j.read
//      _ <- arr.swap(x, r)
//    } yield x
//
//    def qs[S](arr: STArray[S,Int], l: Int, r: Int): ST[S,Unit] = for {
//      p <- partition(arr,l,r,(l+r)/2)
//      _ <- qs(arr,l,p-l)
//      _ <- qs(arr,p+1,r)
//    } yield()
//
//    def quicksort(xs: List[Int]): List[Int] =
//      if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
//        def apply[S] = for {
//          arr    <- STArray.fromList(xs)
//          size   <- arr.size
//          _      <- qs(arr, 0, size - 1)
//          sorted <- arr.freeze
//        } yield sorted
//    })

  }

  import scala.collection.mutable.{HashMap => MHashMap}

    sealed abstract class STHashMap[S,K,V] {
      protected val hmap : MHashMap[K,V]

      def get(k:K) : ST[S,Option[V]] = ST { hmap.get(k) }
      def put(k:K,v:V) : ST[S,Option[V]] = ST { hmap.put(k,v) }
      def exists (k:K) : ST[S,Boolean] = get(k).map { _.isDefined }
      def print : ST[S,Unit] = ST { hmap.foreach { kv => println(s"${kv._1} -> ${kv._2}")} }

      def toImmutableMap  : ST[S,Map[K,V]] = ST { hmap.toMap }  //testing
      }

    }

    object STHashMap {

      import scala.collection.mutable.{HashMap => MHashMap}

      def apply[S,K,V](map: Map[K,V]): STHashMap[S,K,V] = new STHashMap[S,K,V]() {
        override protected val hmap: MHashMap[K, V] = new MHashMap[K,V] ++= map
      }

}