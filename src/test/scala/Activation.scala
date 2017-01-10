package Pacman

import util.Random

import Chisel._

class ActivationTests(c: Activation, p: LayerParameters) extends Tester(c) {
  if (p.MatrixWidth == 0)
    throw new AssertionError("MatrixWidth cannot be zero")

  Random.setSeed(123)

  val MaxNumber = p.MatrixWidth

  for (iter <- 0 until 10) {
    // Generate random input
    val in = List.fill(p.NumberOfPUs) { Random.nextInt(MaxNumber) }
    val expectedResult = in.map((n) => {
        val a = 2 * n - p.MatrixWidth
        if (a >= 0) { 1 } else { 0 }
    })

    // Hook up the inputs
    for (i <- 0 until p.NumberOfPUs) {
      poke(c.io.in(i), in(i))
    }

    for (i <- 0 until p.NumberOfPUs) {
      expect(c.io.out(i), expectedResult(i))
    }
  }
  // Also test extreme cases
  for (i <- 0 until p.NumberOfPUs) {
    poke(c.io.in(i), MaxNumber - 1)
  }
  for (i <- 0 until p.NumberOfPUs) {
    expect(c.io.out(i), 1)
  }
  for (i <- 0 until p.NumberOfPUs) {
    poke(c.io.in(i), 0)
  }
  for (i <- 0 until p.NumberOfPUs) {
    expect(c.io.out(i), 0)
  }
}

object ActivationTest {
  def main(args: Array[String]) {
    val margs = Array("--backend", "c", "--genHarness", "--compile", "--test")
    val p = new LayerParameters(
      K=4,
      BiasWidth=8,
      AccumulatorWidth=10,
      NumberOfPUs=4,
      MatrixWidth=16,
      MatrixHeight=16);

    chiselMainTest(margs, () => Module(new Activation(p))) {
      c => new ActivationTests(c, p)
    }
  }
}
