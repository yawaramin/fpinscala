package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

object Result {
  private type FailedCase = String
  private type SuccessCount = Int

  sealed trait T

  case object Passed extends T

  case object Proved extends T

  case class Falsified(
    failure: FailedCase,
    successes: SuccessCount
  ) extends T

  def isFalsified(t: T): Boolean =
    t match {
      case Passed => false
      case Proved => false
      case Falsified(_, _) => true
    }
}

object Prop {
  import Result.{ Proved, Passed, Falsified }

  private type TestCases = Int
  private type MaxSize = Int

  case class T(run: (MaxSize, TestCases, RNG) => Result.T)

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Option(g.sample.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen.T[A])(f: A => Boolean): T =
    forAll(g.forSize(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): T =
    T { (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props =
        Stream.from(0).take(n.min(max) + 1).map(i => forAll(g(i))(f))
      val prop =
        props.map { p =>
          T { (max, _, rng) => p.run(max, casesPerSize, rng) }
        }.toList.reduce(and)

      prop.run(max, n, rng)
    }

  def forAll[A](as: Gen[A])(f: A => Boolean): T =
    T { (_, n, rng) =>
      Stream
        .zipWith(randomStream(as)(rng), Stream.from(0)) { case (a, b) =>
          a -> b
        }.take(n)
        .map { case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
        }.find(Result.isFalsified(_)).getOrElse(Passed)
    }

  def check(p: => Boolean): T =
    T { (_, _, _) => if (p) Proved else Falsified("()", 0) }

  val S =
    Gen.weighted(
      Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
      Gen.unit(Executors.newCachedThreadPool) -> 0.25
    )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): T =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

  def and(p1: T, p2: T): T =
    T { (max, n, rng) =>
      val r1 = p1.run(max, n, rng)
      val r2 = p2.run(max, n, rng)

      (r1, r2) match {
        case (Proved, Proved) => Proved
        case (Passed, Passed) => Passed
        case (Passed, Proved) => Passed
        case (Proved, Passed) => Passed
        case (Proved, Falsified(_, _)) => r2
        case (Passed, Falsified(_, _)) => r2
        case (Falsified(_, _), _) => r1
      }
    }

  def or(p1: T, p2: T): T =
    T { (max, n, rng) =>
      val r1 = p1.run(max, n, rng)
      val r2 = p2.run(max, n, rng)

      (r1, r2) match {
        case (Proved, _) => Proved
        case (_, Proved) => Proved
        case (Passed, _) => Passed
        case (_, Passed) => Passed
        case (_, _) => r1
      }
    }

  def run(
    t: T,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)
  ): Unit =
    t.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    choose(0, 2).map {
      case 0 => false
      case 1 => true
    }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    def betweenStartStop(rng: RNG): (Int, RNG) = {
      val (i, rng2) = rng.nextInt

      if (start <= i && i < stopExclusive) i -> rng2
      else betweenStartStop(rng2)
    }
    val sample = State(betweenStartStop)

    Gen(sample)
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    g1.flatMap { a1 =>
      g2.flatMap { a2 =>
        boolean.flatMap { chooseA1 => unit(if (chooseA1) a1 else a2) }
      }
    }

  def weighted
    [A]
    (g1: (Gen[A], Double), g2: (Gen[A], Double)):
    Gen[A] = {
    val genDouble = Gen(State(RNG.double))
    val (g1Abs, g2Abs) = (g1._2.abs, g2._2.abs)
    val g1Prob = g1Abs / (g1Abs + g2Abs)

    g1._1.flatMap { a1 =>
      g2._1.flatMap { a2 =>
        genDouble.flatMap { d => unit(if (d >= g1Prob) a1 else a2) }
      }
    }
  }

  def unsized[A](gen: Gen[A]): SGen.T[A] = SGen.T { sz => gen }
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def listOfNDynamic(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(listOfN(_, this))

  def **[B](g: Gen[B]): Gen[(A, B)] = map2(g)(_ -> _)
}

object SGen {
  case class T[A](forSize: Int => Gen[A])

  def map[A, B](t: T[A])(f: A => B): T[B] =
    T(t.forSize andThen (_.map(f)))

  def flatMap[A, B](t: T[A])(f: A => Gen[B]): T[B] =
    T(t.forSize andThen (_.flatMap(f)))

  def unit[A](a: => A): T[A] = Gen.unsized(Gen.unit(a))

  def listOf[A](g: Gen[A]): T[List[A]] = T(Gen.listOfN(_, g))

  def listOf1[A](g: Gen[A]): T[List[A]] =
    T { sz => Gen.listOfN(sz.max(1), g) }
}

object ListProps {
  private val smallInt = Gen.choose(-10, 10)

  val maxProp =
    Prop.forAll(SGen.listOf(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

  val maxProp1 =
    Prop.forAll(SGen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

  val sortedProp =
    Prop.forAll(SGen.listOf(smallInt)) { ns =>
      val nsSorted = ns.sorted

      ns.length == nsSorted.length &&
        (ns.isEmpty ||
          nsSorted.tail.isEmpty ||
          !nsSorted
            .zip(nsSorted.tail)
            .exists { case (a, b) => a > b }
        ) &&
        ns.toSet == nsSorted.toSet
    }

  private def isEven(n: Int) = n % 2 == 0
  private val genListInt = SGen.listOf(smallInt)

  val takeWhileProp = {
    Prop.forAll(genListInt) { ns =>
      ns.takeWhile(_ => true) == ns &&
        ns.takeWhile(_ => false) == List.empty &&
        ns.takeWhile(isEven).forall(isEven)
    }
  }

  val takeWhileDropWhileProp = {
    Prop.forAll(genListInt) { ns =>
      val partitionedLists = ns.partition(isEven)
      partitionedLists == ns.takeWhile(isEven) -> ns.dropWhile(isEven)
    }
  }
}

object ParProps {
  def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p1, p2) { _ == _ }

  def checkPar(p: => Par[Boolean]): Prop.T =
    Prop.forAllPar(Gen.unit(()))(_ => p)

  val pInt = Gen.choose(0, 10).map(Par.unit(_))

  val pIntPar = {
    val genInt = Gen.choose(0, 10)

    genInt.listOfNDynamic(genInt).map { xs =>
      xs.foldLeft(Par.unit(0)) { (p, i) =>
        Par.fork { Par.map2(p, Par.unit(i))(_ + _) }
      }
    }
  }

  val p2 = checkPar(equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2)))

  val p4 = Prop.forAllPar(pInt) { n => equal(Par.map(n)(identity), n) }

  val pFork = Prop.forAllPar(pInt) { n => equal(n, Par.fork(n)) }
}

