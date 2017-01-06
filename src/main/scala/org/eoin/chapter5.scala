package org.eoin

import scala.annotation.tailrec

object Chapter5 {

  object Exercise1 {

    trait MyStream[+A] {
      def uncons:Option[(A, MyStream[A])]
      def isEmpty = uncons == None



      def streamToList3: List[A] = {
        @tailrec
        def innerLoop (ssa: MyStream[A], acc: List[A]) : List[A] = {
          if (ssa.isEmpty) acc.reverse // we're done. reverse explained below
          else {
            val (a, ssaTail) = ssa.uncons.get // safe get as non-empty
            innerLoop (ssaTail, a :: acc) // we are peeling off the heads & pushing onto acc .. therefore need to reverse above when done
          }
        }
        innerLoop (this, List.empty)
      }

      //@tailrec  //doesn't work
      def streamToList: List[A] = {
        uncons match {
          case None => List()
          case Some((h, t)) => h :: t.streamToList
        }
      }


      def take(n: Int): MyStream[A] = {
        uncons match {
          case _ if ( n <= 0 ) => MyStream.empty
          case Some((h, t)) if (n >0) => MyStream.cons(h, t.take(n-1))
          case None => MyStream.empty
        }
      }

      def takeOrig(n: Int): MyStream[A] = {
        require (n >= 0)
        @tailrec
        def innerLoop (input: MyStream[A], acc: List[A], count: Int) : List[A] = {
          if ( count == n || input.isEmpty) acc.reverse  //we're done
          else {
            val (a, ssaTail) = input.uncons.get // safe get as non-empty
            innerLoop(ssaTail, a :: acc, count +1)
          }
        }
        val firstN = innerLoop(this, List.empty,0)
        MyStream(firstN:_*)
      }

      def foldRight[B](z: => B)(f: (A, => B) => B): B =
        uncons match {
          case Some((h, t)) => f(h, t.foldRight(z)(f))
          case None => z
        }

      def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

      def forAll(p: A => Boolean): Boolean = foldRight(true) { (a,b) => p(a ) && b }

      private case class Holder[A](val ms: MyStream[A], val ls: List[A])

      def takeWhile3(p: A => Boolean): MyStream[A] = uncons match {
        case Some(hTail) if p(hTail._1) => MyStream.cons(hTail._1, hTail._2.takeWhile3(p))
        case _ => MyStream.empty
      }

      def takeWhile(p: A => Boolean): MyStream[A] = foldRight(MyStream.empty[A]) { (a,b) =>
        if ( p(a)) MyStream.cons(a, b) else MyStream.empty
      }



      def takeWhile2(p: A => Boolean): MyStream[A] = {
        @tailrec
        def innerLoop (input: MyStream[A], acc: List[A], count: Int) : List[A] = {
          if ( input.isEmpty) acc.reverse  //we're done
          else {
            val (a, ssaTail) = input.uncons.get // safe get as non-empty
            if (! p (a)) acc.reverse  //we're done
            else {
              innerLoop(ssaTail, a :: acc, count +1)
            }
          }
        }
        val firstMatching = innerLoop(this, List.empty,0)
        MyStream(firstMatching:_*)
      }


      def map[B](f: A => B) : MyStream[B] = {
        foldRight(MyStream.empty[B]) { (a, mysB) => MyStream.cons(f(a), mysB) }
      }

      def filter(p: A => Boolean) : MyStream[A] = {
        foldRight(MyStream.empty[A]) { (a, mysB) => if (p(a)) MyStream.cons(a, mysB) else mysB }
      }

      def append[B >: A](sb : => MyStream[B] ) : MyStream[B] = {
        foldRight(sb) { (a, mysB) => MyStream.cons(a, mysB)  }
      }


     def flatMap[B](f: A => MyStream[B]) : MyStream[B] = {
        foldRight(MyStream.empty[B]) { (a, mysB) => f(a) append mysB }
      }


    }





    object MyStream {

      def empty[A] = new MyStream[A] {
        override def uncons: Option[(A, MyStream[A])] = None
      }

      def cons[A] (a: => A, sa: => MyStream[A]) = new MyStream[A] {
        override def uncons: Option[(A, MyStream[A])] = Some(a, sa)
      }

      def apply[A](as: A*) : MyStream[A] = {
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail: _*))
      }

      def constant[A](a: A): MyStream[A] = new MyStream[A] {
        override def uncons: Option[(A, MyStream[A])] = Some(a, this)
      }

      def from(n: Int): MyStream[Int] = new MyStream[Int] {
        override def uncons: Option[(Int, MyStream[Int])] = Some(n, from(n+1))
      }

      def fibs (n: Int): MyStream[Int] = {
        def go ( n1: Int, n2: Int) : MyStream[Int] = {
          MyStream.cons(n1, go(n2, n1+n2))
        }
        go(0,1)
      }


      def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] = {
        f(z) match {
          case Some((a,s)) => cons(a, unfold(s)(f))
          case None => MyStream.empty
        }
      }

      def fromUnfold(n: Int): MyStream[Int] = unfold (0) { s:Int => Option ( (n+s, s+1)) }

      def constantUnfold(n: Int): MyStream[Int] = unfold (0) { s:Int => Option ( (n, s+1)) }

      def conesUnfold(n: Int): MyStream[Int] = unfold (0) { s:Int => Option ( (n, s+1)) }





//     def fibs2 (n: Int): MyStream[Int] = {
//      unfold(1)(i:Int => i match
//        {
//        case @i1 if i >1 =>
//      }
//
//      }


    }
  }
}

