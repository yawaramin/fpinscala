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

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop): Prop =
    new Prop {
      def check =
        Prop.this.check match {
          case Left(a) => Left(a)
          case Right(b) =>
            p.check match {
              case Left(a) => Left(a)
              case Right(b2) => Right(b + b2)
            }
        }
    }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
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
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfNDynamic(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(listOfN(_, this))
}

trait SGen[+A] {

}

