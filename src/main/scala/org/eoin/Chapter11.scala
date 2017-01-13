package org.eoin

import org.eoin.Chapter5.Exercise1.MyStream
import org.eoin.Chapter6.State
import org.scalacheck.{Arbitrary, Gen}

import scala.language.higherKinds

/**
  * Created by eoin.parker on 1/10/17.
  */
object Chapter11 {


  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]

    def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))
  }

  object Functor {

  }

  trait listFunctor extends Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }


  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]
    def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]
    def map[A,B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => unit(f(a)))
    def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    def sequence[A](lma: List[M[A]]): M[List[A]] =
      lma.foldRight (unit(List.empty[A])) {
        (ma,mla) => map2(ma,mla)( (a,mla) => a :: mla)
      }

    def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = {
      la.foldRight (unit(List.empty[B])) {
        (a,mlb) => {
          val mb = f(a)
          map2(mlb,mb) {(lb,b) => b :: lb}
        }
      }
    }

    def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
      val list = List.fill(n)(ma)
      sequence(list)
    }

    def factor[A,B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

    def cofactor[A,B](e: Either[M[A], M[B]]): M[Either[A, B]] = {
      e match {
        case Left(ma) => map(ma) { a => Left(a)}
        case Right(mb) => map(mb) { b => Right(b)}
      }
    }

    def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = {
      (a:A) => flatMap(f(a))(g)

    }

    def flatMapViaCompose [A,B](ma: M[A])(f: A => M[B]): M[B] = ???
  }

  object Monad {
//  val genMonad = new Monad[Gen] {
//    def unit[A](a: => A): Gen[A] = Gen.unit(a)
//    def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
//      ma flatMap f }
//  }

  }

  object exercise1 {

    val optionMonad = new Monad[Option] {
      def unit[A](a: => A): Option[A] = Some(a)
      def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] =
        ma flatMap f
    }

    val mystreamMonad = new Monad[MyStream] {
      def unit[A](a: => A): MyStream[A] = MyStream(a)  //TODO
      def flatMap[A,B](ma: MyStream[A])(f: A => MyStream[B]): MyStream[B] =
        ma flatMap f
    }

    val listMonad = new Monad[List] {
      def unit[A](a: => A): List[A] = List(a)
      def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] =
        ma flatMap f
    }
  }

  object exercise2 {
    type MyIntState[+A] = State[A,Int]
    val stateIntMonad = new Monad[MyIntState] {
      def unit[A](a: => A): MyIntState[A] = State.unit(a)
      def flatMap[A,B](ma: MyIntState[A])(f: A => MyIntState[B]): MyIntState[B] =
        ma flatMap f
    }
  }

  object exercise3 {
//    def sequence[A](lma: List[M[A]]): M[List[A]] = ???
//    def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = ???
  }


  object exercise7 {

    case class Order(item: Item, quantity: Int)
    case class Item(name: String, price: Double)

    implicit val genOrder: Gen[Order] = for {
      name <- Arbitrary.arbitrary[String]
      price <- Arbitrary.arbitrary[Double]
      quantity <- Arbitrary.arbitrary[Int]
    } yield Order(Item(name, price), quantity)

    implicit val genItem: Gen[Item] = for {
      name <- Arbitrary.arbitrary[String]
      price <- Arbitrary.arbitrary[Double]
    } yield Item(name, price)

    implicit val genOrder2: Gen[Order] = for {
      item <- genItem
      quantity <- Arbitrary.arbitrary[Int]
    } yield Order(item, quantity)

    implicit val genOrderExpanded: Gen[Order] = {
      val gs:Gen[String] = Arbitrary.arbitrary[String]
      val gd:Gen[Double] = Arbitrary.arbitrary[Double]
      val gi:Gen[Int] = Arbitrary.arbitrary[Int]

      val go = gs.flatMap{s => {gd.flatMap { d => {gi.map { i => Order(Item(s,d), i)  } } } } }
      go
    }

    implicit val genOrder2Expanded: Gen[Order] = {
      val gItem:Gen[Item] = genItem //Arbitrary.arbitrary[Item]
      val gi:Gen[Int] = Arbitrary.arbitrary[Int]
      val go2 = gItem.flatMap{item => {gi.flatMap { i => Order(item, i)  } } }
      go2
    }

  }

  object exercise19 {

    case class Id[A](value: A) {
      def map[B] (f: A=>B) : Id[B] = new Id(f(value))
      def flatMap[B](f: A=>Id[B]) : Id[B] = f(value)
    }

    implicit val idMonad = new Monad[Id] {
      override def unit[A](a: => A): Id[A] = Id(a)
      override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma.flatMap(f)
    }

    //implicit val


  }

  object exercise20 {

    def stateMonad[S] = new Monad[({type lambda[x] = State[x,S]})#lambda] {
      def unit[A](a: => A): State[A,S] = State(s => (a, s))
      def flatMap[A,B](st: State[A,S])(f: A => State[B,S]): State[B,S] = st.flatMap(f)
    }
  }


}
