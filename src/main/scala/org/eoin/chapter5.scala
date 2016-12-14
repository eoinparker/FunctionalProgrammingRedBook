package org.eoin

import scala.annotation.tailrec

object Chapter5 {

  object Exercise1 {

    trait MyStream[+A] {
      def uncons:Option[(A, MyStream[A])]
      def isEmpty = uncons == None

      def streamToList: List[A] = {
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


      def take(n: Int): MyStream[A] = {
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


      def takeWhile(p: A => Boolean): MyStream[A] = {
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

    }
  }
}

