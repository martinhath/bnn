package Pacman

import Chisel._

class MemoryUnitTests(mu: MemoryUnit) extends Tester(mu) {

  poke(mu.io.restartIn, 1)
  peek(mu.io)
  step(1)
  expect(mu.io.restartOut, 0)
  expect(mu.io.weights, Integer.parseInt("1010", 2))

  poke(mu.io.restartIn, 0)
  peek(mu.io)
  step(1)
  expect(mu.io.restartOut, 1)
  expect(mu.io.weights, Integer.parseInt("0101", 2))

  poke(mu.io.restartIn, 0)
  peek(mu.io)
  step(1)
  expect(mu.io.restartOut, 0)
  expect(mu.io.weights, Integer.parseInt("0011", 2))

  // Go straight to next pass

  poke(mu.io.restartIn, 1)
  peek(mu.io)
  step(1)
  expect(mu.io.restartOut, 0)
  expect(mu.io.weights, Integer.parseInt("1010", 2))

  poke(mu.io.restartIn, 0)
  peek(mu.io)
  step(1)
  expect(mu.io.restartOut, 1)
  expect(mu.io.weights, Integer.parseInt("0101", 2))

  // Restart in the middle of a pass

  poke(mu.io.restartIn, 1)
  peek(mu.io)
  step(1)
  expect(mu.io.restartOut, 0)
  expect(mu.io.weights, Integer.parseInt("1010", 2))

  poke(mu.io.restartIn, 0)
  peek(mu.io)
  step(1)
  expect(mu.io.restartOut, 1)
  expect(mu.io.weights, Integer.parseInt("0101", 2))

  poke(mu.io.restartIn, 0)
  peek(mu.io)
  step(1)
  expect(mu.io.restartOut, 0)
  expect(mu.io.weights, Integer.parseInt("0011", 2))
}

object MemoryUnitTest {
  def main(args: Array[String]) {
    val margs = Array("--backend", "c", "--genHarness", "--compile", "--test")
    val weights = Array[String]("b1010", "b0101", "b0011")
    chiselMainTest(margs, () => Module(new MemoryUnit(weights, 2))) {
      c => new MemoryUnitTests(c)
    }
  }
}
