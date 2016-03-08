package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /*
  This is skewed slightly because it will map both -1 and 0 to 0.
  */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, rng2) = rng.nextInt

    (if (x < 0) -(x + 1) else x, rng2)
  }

  /*
  Repeatedly generating random numbers until we get one that fits the
  properties we want is less biased than mapping multiple numbers from
  the domain into one number in the range.
  */
  @annotation.tailrec
  def double(rng: RNG): (Double, RNG) = {
    val zMax = Int.MaxValue
    val (x, rng2) = nonNegativeInt(rng)

    if (x == zMax) double(rng2) else (x.toDouble / zMax, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)

    i -> d -> rng3
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)

    d -> i -> rng2
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)

    (d1, d2, d3) -> rng4
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(count: Int, z: (List[Int], RNG)): (List[Int], RNG) =
      if (count < 1) z
      else {
        val (x, rng2) = z._2.nextInt
        go(count - 1, (x :: z._1) -> rng2)
      }

    go(count, List.empty[Int] -> rng)
  }

  def double_via_map: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)

      f(a, b) -> rng3
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) { (ra, rb) =>
      map2(ra, rb) { (a, b) => a :: b }
    }

  def ints_via_sequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count) { rng2: RNG => rng2.nextInt })(rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { a =>
      val mod = a % n
      if (a + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def map_via_flatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(f andThen unit)

  def map2_via_flatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map_via_flatMap(rb)(b => f(a, b)))
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(f andThen State.unit)

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def input(i: Input): Machine =
    (i, this) match {
      case (_, Machine(_, 0, _)) => this
      case (Coin, Machine(true, candies, coins)) =>
        Machine(false, candies, coins + 1)

      case (Turn, Machine(false, candies, coins)) =>
        Machine(true, candies - 1, coins)

      case (Turn, Machine(true, _, _)) => this
      case (Coin, Machine(false, candies, coins)) =>
        Machine(false, candies, coins + 1)
    }
}

object State {
  /*
  Applicative seems to be the wrong place for these to be defined
  (because they're introduced in Chapter 6 of the book), so for now just
  grab them.
  */
  import fpinscala.applicative.StateUtil.{ get, set }

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(a -> _)

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](List.empty)) { (sa, sb) =>
      sa.map2(sb) { (a, b) => a :: b }
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence {
        inputs map { i: Input =>
          modify { m: Machine => m input i }
        }
      }

      m <- get
    } yield (m.coins, m.candies)
}
