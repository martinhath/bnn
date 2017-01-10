package Pacman

import Chisel._

class PacmanSpeedTestHarnessTests(
  c: PacmanSpeedTestHarness
) extends Tester(c) {
  for(i <- 0 until 100000) {
    peek(c.io)
    peek(c.totalCounter.io)
    peek(c.incorrectSwitch.io)
    peek(c.resultReg)
    peek(c.correctResultReg)
    peek(c.pacman.net.io.done)
    step(1)
  }
}


object PacmanSpeedTestHarnessTest {
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
    val inData = testInputs.flatten.grouped(inputStreamWordWidth).toArray
    val outData = testOutputs.map(_.indexWhere(_ == 1)).map(a => if(a == -1) 0 else a).toArray

    println(inData.deep)
    println()
    println(outData.deep)



    chiselMainTest(margs, () => Module(new PacmanSpeedTestHarness(
                                         layers,
                                         inData,
                                         outData,
                                         50
                                       ))) {
      c => new PacmanSpeedTestHarnessTests(c)
    }
  }
}
