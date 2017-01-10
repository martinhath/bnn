package Pacman

import Chisel._

class ProcessingUnitTests(c: ProcessingUnit, p: LayerParameters) extends Tester(c) {
  val ones = (Math.pow(2, p.K) - 1).toInt
  poke(c.io.xs, 0)
  poke(c.io.ws, ones)
  poke(c.io.restartIn, true)
  poke(c.io.bias, 0)

  step(1)

  expect(c.io.xOut, 0)
  expect(c.io.yOut, 0)
  expect(c.io.restartOut, true);
  poke(c.io.restartIn, false);
  poke(c.io.xs, ones)
  poke(c.io.bias, 123)

  step(1)

  expect(c.io.xOut, ones)
  expect(c.io.yOut, p.K)
  expect(c.io.restartOut, false);
  poke(c.io.ws, 0)
  poke(c.io.bias, 321)

  step(1)

  expect(c.io.xOut, ones)
  expect(c.io.yOut, p.K)
  expect(c.io.restartOut, false);
  poke(c.io.ws, ones)
  poke(c.io.bias, 5125)

  step(1)

  expect(c.io.xOut, ones)
  expect(c.io.yOut, p.K * 2)
  expect(c.io.restartOut, false);

  poke(c.io.restartIn, true)
  poke(c.io.ws, ones)
  poke(c.io.xs, ones)
  poke(c.io.bias, -10)

  step(1)

  expect(c.io.xOut, ones)
  expect(c.io.yOut, p.K - 10)
  expect(c.io.restartOut, true);
  poke(c.io.xs, 0)

  step(1)

  expect(c.io.yOut, -10)
  expect(c.io.xOut, 0)
  expect(c.io.restartOut, true);

  val upperHalf = ones >> (p.K / 2)
  val lowerHalf = ~upperHalf

  poke(c.io.xs, upperHalf)
  poke(c.io.ws, lowerHalf)
  poke(c.io.restartIn, false)

  step(1)

  expect(c.io.xOut, upperHalf)
  expect(c.io.yOut, -10)
  expect(c.io.restartOut, false);

}

object ProcessingUnitTest {
  def main(args: Array[String]) {
    val margs = Array("--backend", "c", "--genHarness", "--compile", "--test")
    val p = new LayerParameters(K=4, BiasWidth=8, AccumulatorWidth=10);
    chiselMainTest(margs, () => Module(new ProcessingUnit(p))) {
      c => new ProcessingUnitTests(c, p)
    }
  }
}
