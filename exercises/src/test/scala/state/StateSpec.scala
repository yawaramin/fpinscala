import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

import fpinscala.state._

class StateSpec extends Specification with ScalaCheck {
  def rngFromSeed(seed: Long) = RNG.Simple(seed)
  def betweenZeroAndOneEx(x: Double) =
    x must beBetween(0.0, 1.0).excludingEnd
  val machine = Machine(true, 5, 10)

  "nonNegativeInt" should {
    "always return a non-negative integer" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val randNum = RNG.nonNegativeInt(rng)._1

        randNum must beBetween(0, Int.MaxValue)
      }
    }
  }

  "double" should {
    "always return a non-negative double less than 1" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val randDbl = RNG.double(rng)._1

        betweenZeroAndOneEx(randDbl)
      }
    }
  }

  "intDouble" should {
    "always return an int and a non-negative double less than 1" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val ((i, d), _) = RNG.intDouble(rng)

        i must beBetween(Int.MinValue, Int.MaxValue) and {
          betweenZeroAndOneEx(d)
        }
      }
    }
  }

  "doubleInt" should {
    "always return a non-negative double less than 1 and an int" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val ((d, i), _) = RNG.doubleInt(rng)

        i must beBetween(Int.MinValue, Int.MaxValue) and {
          betweenZeroAndOneEx(d)
        }
      }
    }
  }

  "double3" should {
    "always return three non-negative doubles less than 1" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val ((d1, d2, d3), _) = RNG.double3(rng)

        betweenZeroAndOneEx(d1) and betweenZeroAndOneEx(d1) and {
          betweenZeroAndOneEx(d3)
        }
      }
    }
  }

  "ints" should {
    "return an empty list" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)

        RNG.ints(0)(rng)._1 mustEqual List.empty[Int]
      }
    }

    "return a non-empty list" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val length = 10

        RNG.ints(length)(rng)._1.length mustEqual length
      }
    }
  }

  "double_via_map" should {
    "always return a non-negative double less than 1" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val randDbl = RNG.double_via_map(rng)._1

        betweenZeroAndOneEx(randDbl)
      }
    }
  }

  "map2" should {
    "combine two random actions into one" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)

        betweenZeroAndOneEx(
          RNG.map2(RNG.double_via_map, RNG.double_via_map)(_ * _)(rng)._1
        )
      }
    }
  }

  "sequence" should {
    "join an empty list into a random state transition over an empty list" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val l = List.empty[RNG.Rand[Int]]

        RNG.sequence(l)(rng)._1 mustEqual l
      }
    }

    "join a list of random state transitions into a random state transition over a list" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val l = List(1, 2, 3)

        RNG.sequence(l.map(RNG.unit))(rng)._1 mustEqual l
      }
    }
  }

  "ints_via_sequence" should {
    "return an empty list" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)

        RNG.ints_via_sequence(0)(rng)._1 mustEqual List.empty[Int]
      }
    }

    "return a non-empty list" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val length = 10

        RNG.ints_via_sequence(length)(rng)._1.length mustEqual length
      }
    }
  }

  "nonNegativeLessThan" should {
    "always return a positive number between 0 and n" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val n = 100

        RNG.nonNegativeLessThan(n)(rng)._1 must {
          beBetween(0, n).excludingEnd
        }
      }
    }
  }

  "map_via_flatMap" should {
    "map over a random state transition" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)
        val x = 1

        RNG.map_via_flatMap(RNG.unit(x))(_ + 1)(rng)._1 mustEqual {
          x + 1
        }
      }
    }
  }

  "map2_via_flatMap" should {
    "combine two random actions into one" in {
      prop { seed: Long =>
        val rng = rngFromSeed(seed)

        betweenZeroAndOneEx(
          RNG.map2_via_flatMap(RNG.double_via_map, RNG.double_via_map)(_ * _)(rng)._1
        )
      }
    }
  }

  "State.unit" should {
    "insert a value into an arbitrary state transition" in {
      prop { (x: Int, s: Int) => State.unit(x).run(s)._1 mustEqual x }
    }
  }

  "State.map" should {
    "map a value within an arbitrary state transition" in {
      prop { (x: Int, s: Int) =>
        def add1(x: Int) = x + 1
        State.unit(x).map(add1).run(s)._1 mustEqual add1(x)
      }
    }
  }

  "State.map2" should {
    "map over two values within an arbitrary state transition" in {
      prop { (x1: Int, x2: Int, s: Int) =>
        def add(x1: Int, x2: Int) = x1 + x2
        State.unit[Int, Int](x1).map2(State.unit[Int, Int](x2))(add).run(s)._1 mustEqual add(x1, x2)
      }
    }
  }

  "State.sequence" should {
    "string together a list of values within arbitrary states into a list of values within a single arbitrary state" in {
      prop { (xs: List[Int], s: Int) =>
        State.sequence(xs.map(State.unit[Int, Int])).run(s)._1 mustEqual {
          xs
        }
      }
    }
  }

  "State.simulateMachine" should {
    "not change the state if given no input" in {
      State.simulateMachine(List.empty[Input]).run(machine)._1 mustEqual {
        10 -> 5
      }
    }

    "succeed if given invalid input" in {
      State.simulateMachine(
        List(
          Turn, Turn,
          Coin, Coin
        )
      ).run(machine)._1 mustEqual (12 -> 5)
    }

    "succeed at running simulation in book" in {
      State.simulateMachine(
        List(
          Coin, Turn,
          Coin, Turn,
          Coin, Turn,
          Coin, Turn
        )
      ).run(machine)._1 mustEqual (14 -> 1)
    }
  }
}

