package Pacman

import Chisel._
import util.Random

class PacmanTests(
  c: Pacman,
  testInputWords: Array[Array[Int]],
  testOutputs: Array[Array[Int]]
) extends Tester(c, isTrace=false) {
  def peeks() {
    peek(c.widthConverter.io)
    peek(c.interleaver.io)
    peek(c.interleaver.queues(0).io)
    peek(c.net.io)
    peek(c.deinterleaver.io)
  }
  def vecToBigInt(vec: Seq[Int]): BigInt = {
    int(Bits(vec.reverse.map((n) => n.toString).fold("b")(_ + _)))
  }

  val layers = c.layers
  val RANDOM_WAITS = true


  poke(c.io.inDataStream.valid, false)
  poke(c.io.digitOut.ready, true)

  step(1000)

  var inputWordI = 0
  var outputI = 0
  var correctImages = 0
  var totalImages = 0
  var epochs = 0
  var cycles = 0
  while(epochs < 2) {
    val ready = peek(c.io.inDataStream.ready)
    val validOutput = peek(c.io.digitOut.valid)
    if(validOutput == 1) {
      val digit = peek(c.io.digitOut.bits)
      val correctDigitRaw = testOutputs(outputI).indexWhere(_ == 1)
      val correctDigit = if(correctDigitRaw == -1) 0 else correctDigitRaw
      println("%d should be %d".format(digit, correctDigit))
      expect(c.io.digitOut.bits, correctDigit)
      outputI = (outputI + 1) % testOutputs.length
      if(outputI == 0) {
        epochs += 1
      }
      totalImages += 1
      if(digit == correctDigit) {
        correctImages += 1
      }
      println("%d / %d correct".format(correctImages, totalImages))
    }

    if(ready == 1) {
      if(RANDOM_WAITS && Random.nextInt(2) == 0) {
        poke(c.io.inDataStream.valid, false)
        poke(c.io.inDataStream.bits, Random.nextInt(12345678))
      }
      else {
        poke(c.io.inDataStream.valid, true)
        poke(c.io.inDataStream.bits, vecToBigInt(testInputWords(inputWordI)))
        inputWordI = (inputWordI + 1) % testInputWords.length
      }
    }
    else {
      poke(c.io.inDataStream.valid, false)
      poke(c.io.inDataStream.bits, Random.nextInt(12345678))
    }

    peeks()
    step(1)
    cycles += 1
  }

  println("Total cycles: %d".format(cycles))
  println("Total images: %d".format(totalImages))
  println("Cycles per image: %f".format(cycles.toFloat / totalImages))

}

object PacmanTest {
  def main(args: Array[String]) {
    val margs = Array("--backend", "c", "--genHarness", "--compile", "--test", "--debug")

    val testData = Utils.readDumpFile()
    val testInputs = testData.vectors.map(_(0)).toArray
    val testOutputs = testData.vectors.map(_(4)).toArray
    val parametersList = Array(
      new LayerParameters(
        K = 16,
        BiasWidth = 8,
        AccumulatorWidth = 10,
        NumberOfPUs = 32,
        AddressWidth = 0,
        NumberOfMS = 16,
        MatrixWidth = 784,
        MatrixHeight = 256,
        NumberOfCores = 3
      ),
      new LayerParameters(
        K = 16,
        BiasWidth = 8,
        AccumulatorWidth = 10,
        NumberOfPUs = 16,
        AddressWidth = 0,
        NumberOfMS = 8,
        MatrixWidth = 256,
        MatrixHeight = 256,
        NumberOfCores = 3
      ),
      new LayerParameters(
        K = 16,
        BiasWidth = 8,
        AccumulatorWidth = 10,
        NumberOfPUs = 16,
        AddressWidth = 0,
        NumberOfMS = 8,
        MatrixWidth = 256,
        MatrixHeight = 256,
        NumberOfCores = 3
      ),
      new LayerParameters(
        K = 16,
        BiasWidth = 8,
        AccumulatorWidth = 10,
        NumberOfPUs = 10,
        AddressWidth = 0,
        NumberOfMS = 5,
        MatrixWidth = 256,
        MatrixHeight = 10,
        NumberOfCores = 1
      )
    )
    val layers = Range(0, 4).map(i =>
      new LayerData(
        parameters=parametersList(i),
        weights=testData.matrices(i),
        biases=testData.biases(i)
      )
    ).toList


    val inputStreamWordWidth = 8 // We get bytes in

    val inputCores = layers(0).parameters.NumberOfCores
    val inputK = layers(0).parameters.K
    val outputCores = layers.last.parameters.NumberOfCores
    val chunksInImage = layers(0).parameters.MatrixWidth / inputK
    val kChunkedTestInputs = testInputs.map(_.grouped(inputK).toArray).toArray
    val warpGrouped = kChunkedTestInputs.grouped(inputCores).map(_.transpose.toArray).toArray
    val netInputWordArrays = warpGrouped.map(_.map(_.flatten.toArray).toArray).flatten.toArray
    val inputStreamWordArrays = netInputWordArrays.flatten.grouped(inputStreamWordWidth).toArray

    // println(netInputWordArrays.map(_.slice(0, 16).toArray)
    //   .flatten
    //   .grouped(28)
    //   .map(_.toArray.mkString)
    //   .toArray.deep.mkString("\n")
    // )

    // println(inputStreamWordArrays.deep.mkString("\n"))

    val input = testInputs.flatten.grouped(inputStreamWordWidth).toArray

    chiselMainTest(margs, () => Module(new Pacman(inputStreamWordWidth, layers))) {
      c => new PacmanTests(c, input, testOutputs)
    }
  }
}
