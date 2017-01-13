package org.eoin

import org.eoin.Chapter10.exercise9.{Part, Stub, WC}


object Chapter10 {

  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  object exercise1 {

    val stringMonoid = new Monoid[String] {
      def op(a1: String, a2: String) = a1 + a2
      def zero = ""
    }
    def listMonoid[A] = new Monoid[List[A]] {
      def op(a1: List[A], a2: List[A]) = a1 ++ a2
      def zero = Nil
    }

    val intAddition: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1+a2
      override def zero: Int = 0
    }
    val intMultiplication: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 * a2
      override def zero: Int = 1
    }
    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 | a2
      override def zero: Boolean = false
    }
    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 & a2
      override def zero: Boolean = true
    }
  }

  object exercise2 {
    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
      override def zero: Option[A] = None
    }
  }

  object exercise3 {
     def EndoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
       override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 andThen a2
       override def zero: (A) => A = identity
     }
  }

    object exercise5 {
      def wordsMonoid(s: String): Monoid[String]= new Monoid[String] {
        override def op(a1: String, a2: String): String = {
          val s1 = s"${a1.trim}${s}${a2.trim}"
          s1.trim
        }
        override def zero: String = ""
      }
    }

  object exercise6 {
    def concatenate[A](as: List[A], m: Monoid[A]): A = {
      as.fold(m.zero)(m.op)
    }
  }

  object exercise7 {
    def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
      exercise6.concatenate(as map f, m)
    }
  }
  object exercise9 {
    sealed trait WC
    case class Stub(chars: String) extends WC
    case class Part(lStub: String, words: Int, rStub: String) extends WC

    def wcMonoid: Monoid[WC]= new Monoid[WC] {

      def isWordSeparator(c:Char) = c.isWhitespace || ",.-()[]';:".contains (c)

      override def op(a1: WC, a2: WC): WC = (a1,a2) match {
        case (Stub(c1),Stub(c2)) => Stub(c1 + c2)
        case (Stub(c), Part(ls,w,rs)) =>
          val lsCombo = c+ls
          val newWordCount = w + lsCombo count { isWordSeparator _ } // incorrect, double counts eg " . "
          val newLs = (lsCombo).takeWhile(! isWordSeparator(_))
          Part(newLs, newWordCount, rs)
        case (Part(ls,w,rs), Stub(c)) =>
          val rsCombo = rs+c
          val newWordCount = w + rsCombo count { isWordSeparator _ }
          val newRs = (rsCombo).takeWhile(! isWordSeparator(_))
          Part(ls, newWordCount, newRs)
        case (Part(ls1,w1,rs1), Part(ls2,w2,rs2)) =>
          val midCombo = rs1+ls2
          val newWordCount = w1+w2 + (rs1+ls2).count(isWordSeparator(_))
          Part(ls1, newWordCount, rs2)
      }
      override def zero: WC = Stub("")
    }

  }

  object exercise10 {
    def countWords (s:String):Int = {
      val wcMonoid = exercise9.wcMonoid
      def toWC(c: Char): WC = {
        if (c.isWhitespace) Part("", 0, "")
        else Stub(c.toString)
      }
      def unstub(s: String) = s.length min 1

      exercise7.foldMap(s.toList, wcMonoid)(toWC) match {
        case Stub(s) => unstub(s)
        case Part(l, w, r) => unstub(l) + w + unstub(r)
      }
    }
  }

    object exercise11 {
      //@tailrec
      def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
        v.length match {
          case 0 => m.zero
          case 1 => f(v(0))
          case n =>
            val (left,right) = v.splitAt( n/2 )
            m.op( foldMapV(left, m)(f), foldMapV(right, m)(f) )
        }
      }
    }

      object exercise12 {

        type IntBool = (Int,Boolean)
        val compareMonoid = new Monoid[IntBool] {
          override def op(a1: IntBool, a2: IntBool): IntBool = {
            val (a1Int,a1Bool) = a1
            val (a2Int,a2Bool) = a2
            val unorderingDetected = a1Bool || a2Bool || (a2Int < a1Int)
            (Math.abs(a2Int-a1Int), unorderingDetected)
          }
          override def zero = (0,false)
        }

        def isOrdered (is: IndexedSeq[Int]) : Boolean = {
          val f = (i:Int) => (i,false)
          val t = exercise11.foldMapV(is,compareMonoid) ( f)
          ! t._2
        }
      }

  object foldable {
    import scala.language.higherKinds

    trait Foldable[F[_]] {
      def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
      def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
      def foldMap[A, B](as: F[A])(z: B)(f: A => B)(mb: Monoid[B]): B
      def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
    }

    object foldableList extends Foldable[List] {
      override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
      override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
      override def foldMap[A, B](as: List[A])(z: B)(f: (A) => B)(mb: Monoid[B]): B = exercise11.foldMapV(as.toIndexedSeq,mb)(f)
    }

    object foldableIndexedSeq extends Foldable[IndexedSeq] {
      override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
      override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
      override def foldMap[A, B](as: IndexedSeq[A])(z: B)(f: (A) => B)(mb: Monoid[B]): B = exercise11.foldMapV(as,mb)(f)
    }

    sealed trait Tree[+A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    case class Leaf[A](value: A) extends Tree[A]

    object foldableTree extends Foldable[Tree] {
      override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = ???
      override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = ???
      override def foldMap[A, B](as: Tree[A])(z: B)(f: (A) => B)(mb: Monoid[B]): B = ???
    }

    object foldableOption extends Foldable[Option] {
      override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = ???
      override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = ???
      override def foldMap[A, B](as: Option[A])(z: B)(f: (A) => B)(mb: Monoid[B]): B = ???
    }
  }

}