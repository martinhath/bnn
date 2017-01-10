package Pacman

import util.Random

import Chisel._

class ChainTests(c: Chain,
                 processingUnits: Int,
                 k: Int,
                 matrixDimensions: (Int, Int)
  ) extends Tester(c) {

  def posMod(n: Int, a: Int): Int = {
    (n + a) % a
  }

  val N_ITERS = 3
  Random.setSeed(1234)
  val (matHeight, matWidth) = matrixDimensions
  val weights = Matrix.getWeightMatrix(matWidth, matHeight)

  val totalSteps = (matWidth * matHeight) / (k * processingUnits)
  val offsetArray =
    List.range(0, matHeight / processingUnits)
      .flatMap((i) => List.range(0, matWidth / k)
                  .map((n) => n * k + i * matWidth * processingUnits))

  def vecToBigInt(vec: Seq[Int]): BigInt = {
    int(Bits(vec.reverse.map((n) => n.toString).fold("b")(_ + _)))
  }

  var result = Seq(1)
  // For each input vector
  for (iteration <- 0 until N_ITERS) {
    val xs = Matrix.getInputVector(matWidth)
    val previousRes = result
    result = Matrix.matrixMul(weights, xs, matWidth, matHeight)

    // Matrix.printMat(xs, 1, matWidth);
    // Matrix.printMat(weights, matHeight, matWidth);
    // Matrix.printMat(result, 1, matHeight);

    // One step is reading `k` x values. After matWidth/k
    // steps we get out the first y.
    for (stepNumber <- 0 until totalSteps) {
      // The index along the x axis of the matrix
      val rowPassLength = matWidth / k
      val rowPassIndex = stepNumber % rowPassLength
      val whichRowPass = stepNumber / rowPassLength
      val passHeight = processingUnits
      val numPasses = matHeight / passHeight

      // Check Y outputs from previous cyclepassstep
      if (iteration != 0 && rowPassIndex < processingUnits) {
        val resultToCheckAgainst = if (whichRowPass == 0) previousRes else result
        val whichRowPassToCheck = posMod(whichRowPass - 1, numPasses)
        val i = whichRowPassToCheck * passHeight + rowPassIndex
        val bias = posMod(stepNumber - matWidth / k, totalSteps) - 5
        expect(c.io.ys(rowPassIndex),
          resultToCheckAgainst(i) + bias)
      }

      for (puIndex <- 0 until processingUnits) {
        var num = new Array[Int](k)
        val offsetIndex = posMod(stepNumber - puIndex, totalSteps)
        val weightIndex = offsetArray(offsetIndex) + matWidth * puIndex
        for (i <- 0 until k) {
          num(i) = weights(weightIndex + i)
        }
        poke(c.io.weights(puIndex), vecToBigInt(num))
      }

      val xIndex = k * (stepNumber % (matWidth / k))
      poke(c.io.xs, vecToBigInt(xs.slice(xIndex, xIndex + k)))

      // If we are to start a new row pass, reset the chain
      poke(c.io.bias, stepNumber - 5)
      if (rowPassIndex == 0) {
        poke(c.io.restartIn, true)
      } else if (rowPassIndex == 1) {
        poke(c.io.restartIn, false)
      }

      step(1)
    }
  }
}

object ChainTest {

  def divisors(n: Int): Seq[Int] = {
    List.range(1, n + 1).filter((e) => n % e == 0)
  }

  def test(matHeight: Int,
           matWidth: Int,
           chainLength: Int,
           k: Int) = {
    val margs = Array("--backend", "c", "--genHarness",
                      "--compile", "--test")
    println("matHeight", matHeight)
    println("matWidth", matWidth)
    println("chainLength", chainLength)
    println("k", k)

    val p = new LayerParameters(
      K=k,
      BiasWidth=18,
      AccumulatorWidth=10,
      NumberOfPUs=chainLength,
      NumberOfMS=chainLength
      )

    chiselMainTest(margs, () =>
        Module(new Chain(p))) {
      c => new ChainTests(c,
                          chainLength,
                          k,
                          (matHeight, matWidth))
    }
  }

  def main(args: Array[String]): Unit = {
    for (matHeight <- List(4, 8, 10, 28)) {
      for (matWidth <- List(8, 16, 24, 64)) {
        val possibleKs = divisors(matWidth);
        for (k <- possibleKs) {

          val possibleLengths = List.range(1, Math.min(matHeight, matWidth / k))
            .filter((l) => matHeight % l == 0)

          for (chainLength <- possibleLengths) {
            test(matHeight, // matHeight
                 matWidth, // matWidth
                 chainLength, // numPUs
                 k) // k
          }
        }
      }
    }
  }
}
