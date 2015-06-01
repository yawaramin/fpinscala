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
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

