package org.eoin

import scala.annotation.tailrec


object Chapter6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed*0x5DEECE66DL + 0xBL) &
                ((1L << 48) - 1)

        ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
      }
    }
  }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] =  rng => (a, rng)  // no change
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    def foo (rng: RNG) : (B, RNG) = {
      val (a,rng2) = s(rng)
      ( f(a), rng2)
    }
    foo
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    def foo (rng: RNG) : (C, RNG) = {
      val (a,rng1) = ra(rng)
      val (b,rng2) = rb(rng1)
      ( f(a,b), rng2)
    }
    foo
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {

    def foldFn (randa: Rand[A], tuple: (List[A],RNG )) : (List[A], RNG) = {
      val (ls,rng) = tuple
      val (a, newRngState) = randa(rng)
      (a :: ls, newRngState)
    }

    // here is the function we wish to construct & return
    rng:RNG => fs.foldRight( (List.empty[A],rng) ) { foldFn } // starting input to the fold is a tuple

  }

  def flatMap[A,B](ra: Rand[A])(g: A => Rand[B]): Rand[B] = {
    (rng:RNG) => {
        val (a,nextRng) = ra(rng)
        val ga = g(a)
        ga(nextRng)
    }
  }


  object exercise1 {

    def positiveInt(rng: RNG): (Int, RNG) = {
      val (nextI, nextRNG) = rng.nextInt
      (nextI.abs, nextRNG)
    }
  }


  object exercise2 {

    def double(rng: RNG): (Double, RNG) = {
      val (posI, nextRNG) = exercise1.positiveInt(rng)
      ( posI.toDouble / Int.MaxValue, nextRNG)
    }

  }

  object exercise3 {

    def intDouble(rng: RNG): ((Int,Double), RNG) = {
      val (posI, rng1) = exercise1.positiveInt(rng)
      val (doub, rng2)= exercise2.double(rng1)
      ((posI, doub), rng2)
    }
    def doubleInt(rng: RNG): ((Double,Int), RNG) = {
      val id = intDouble(rng)
      (id._1.swap, id._2)
    }
    def double3(rng: RNG): ((Double,Double,Double), RNG) = {
      val (doub1, rng1) = exercise2.double(rng)
      val (doub2, rng2)= exercise2.double(rng1)
      val (doub3, rng3)= exercise2.double(rng2)
      ((doub1,doub2,doub3), rng3)
    }

  }

  object exercise5 {
    def positiveMax(n: Int): Rand[Int] = map ( exercise1.positiveInt ) { _ % n}
  }

  object exercise6 {
    def double: Rand[Double] = map ( exercise1.positiveInt ) { _ / Int.MaxValue}
  }

  object exercise7 {
    def intDouble: Rand[(Int,Double)] = map2 (exercise1.positiveInt, exercise2.double) { (a,b) => Tuple2(a,b) }
    def doubleInt: Rand[(Double,Int)] = map2 (exercise2.double, exercise1.positiveInt) { (a,b) => Tuple2(a,b) }
  }

  object exercise8 {

//     def ints(count: Int)(rng: RNG): Rand[List[Int]] = {
//       sequence()
//     }


  }

  object exercise9 {

    def positiveInt: Rand[Int] = {
      flatMap(int) { (i:Int) =>
        if (i != Int.MinValue) { rng => (i.abs,rng)}
        else { positiveInt }
      }
    }
  }

  object exercise10 {
    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s) { (a:A) =>  unit(f(a)) }

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra) { (a:A) => flatMap(rb) {(b:B) => unit(f(a,b))} }

    def double: Rand[Double] = exercise10.map ( exercise1.positiveInt ) { _ / Int.MaxValue}
    def intDouble: Rand[(Int,Double)] = exercise10.map2 (exercise1.positiveInt, exercise2.double) { (a,b) => Tuple2(a,b) }
    def doubleInt: Rand[(Double,Int)] = exercise10.map2 (exercise2.double, exercise1.positiveInt) { (a,b) => Tuple2(a,b) }
  }


  case class State[+A,S](run: S => (A,S)) {

    def flatMap[B](g: A => State[B,S]): State[B,S] = {
      def newRunFn (s:S): (B,S) = {
        val (a,s1) = run(s)
        g(a).run (s)
      }
      State(newRunFn)
    }

    def map[B](f: A => B): State[B,S] = flatMap( {(a:A) => State.unit(f(a))} )

    def map2[B,C](sb: State[B,S])(f: (A, B) => C): State[C,S] = {
      val newRunFn = (s:S) => {
        val (a,s1) = this.run (s)
        val (b,s2) = sb.run(s1)
        val c = f(a,b)
        (c,s2)
      }
      State(newRunFn)
      }

    def modify(f: S => S): State[Unit, S] = for {
      s <- get
      _ <- set(f(s))
    } yield ()

    def get: State[S,S]  = State( (s) => (s,s))
    def set (newS:S): State[Unit,S]  = State( (s:S) => ((),newS))

  }

  object State {
      def unit[A,S](a: A): State[A,S] = new State( s => (a,s) )

      def sequence[A,S](ls: List[State[A,S]]): State[List[A],S] = {
        val run: (S) => (List[A], S) = { (s) =>
          ls.foldRight( (List.empty[A],s) ) ( (state,tuple) => {
            val (list, s0) = (tuple._1, tuple._2)
            val (a,s1) = state.run(s0)
            (a :: list, s1)
            }
          )
        }
        new State (run)
      }

  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, Machine ] = {


    def applyInput(input:Input)(machine:Machine) : State[Machine, Machine] = {
      val outputMachine = input match {
        case _ if (machine.candies <= 0) => machine
        case Coin if (machine.locked && machine.candies>0) => machine.copy(locked=false, coins = machine.coins+1)
        case Turn if (! machine.locked && machine.candies>0) => machine.copy(locked=true, candies = machine.candies-1)
        case Turn if (machine.locked) => machine
        case Coin if (!machine.locked) => machine
      }
      println(s"new state: ${outputMachine}")
      State( (s) => (outputMachine, outputMachine ) )
    }

    def applySpecificInput(input:Input) : (Machine => State[Machine, Machine]) = applyInput(input) _

    @tailrec
    def loop (inputs: List[Input], acc: State[Machine, Machine]) : State[Machine, Machine] = {
      inputs match {
        case Nil => acc
        case head :: tail => {
          val func = applySpecificInput(head)
          val newState = acc.flatMap[Machine](func)
          loop(tail,newState)
        }
      }
    }

    val initialState = State[Machine, Machine ](m => (m, m))   // identity
    loop (inputs,initialState)
  }



}