package Pacman

import Chisel._

class NetLedHarnessTests(c: NetLedHarness) extends Tester(c) {
  step(3000)
  for(i <- 0 until 100) {
    peek(c.io)
    // peek(c.inputCycleCounter.io)
    // peek(c.waitCounter.io)
    // peek(c.currentInput.io)
    // peek(c.bitBuffer.io.word)
    // peek(c.net.warps(0).io)
    // peek(c.net.warps(0).memoryStreamer.io)
    // peek(c.net.warps(0).chains(0).processingUnits(0).io)
    // peek(c.net.warps(1).io)
    // peek(c.net.warps(2).io)
    // peek(c.net.warps(3).io)
    // peek(c.net.gearBoxes(0).io)
    // peek(c.net.gearBoxes(1).io)
    // peek(c.net.gearBoxes(2).io)
    step(100)
  }
}

object NetLedHarnessTest {
  def main(args: Array[String]) {
    val margs = Array("--backend", "v", "--genHarness", "--compile", "--test")

    val testData = Utils.readDumpFile()
    val testInput = testData.vectors.map(_(0)).toArray
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
        NumberOfCores = 4
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
        NumberOfCores = 2
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
        NumberOfCores = 2
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

    chiselMainTest(margs, () => Module(new NetLedHarness(layers, testInput))) {
      c => new NetLedHarnessTests(c)
    }
  }
}
