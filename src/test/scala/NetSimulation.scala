package Pacman

import Chisel._

class NetSimulationHarnessTests(
  c: NetSimulationHarness,
  layers: List[LayerData],
  testInputs: Array[Array[Int]],
  testOutputs: Array[Array[Int]]
) extends Tester(c, isTrace=false) {
  def peeks() {
    return
    peek(c.net.io)
    peek(c.net.gearBoxes(0).io)
    for (i <- 0 until 4)
      peek(c.net.gearBoxes(0).inputSelectCounters(i).io)
    peek(c.net.gearBoxes(0).bitCounter.io)
    for (i <- 0 until 2)
      peek(c.net.gearBoxes(0).queueOutputSelectCounters(i).io)
    for (i <- 0 until 4)
      peek(c.net.gearBoxes(0).outputSelectCounters(i).io)
    for (i <- 0 until 4)
      peek(c.net.gearBoxes(0).queues(i).io)
    peek(c.net.warps(0).io)
    // peek(c.net.warps(1).io)
    // peek(c.net.warps(2).io)
    // peek(c.net.warps(3).io)
    // peek(c.outputCounter.io)
  }

  val inputCores = layers(0).parameters.NumberOfCores
  val inputK = layers(0).parameters.K
  val outputCores = layers.last.parameters.NumberOfCores
  val chunksInImage = layers(0).parameters.MatrixWidth / inputK
  val groupedTestInputs = testInputs.grouped(inputCores)
    .map(testGroup => testGroup.map(test => test.grouped(inputK).toArray))
    .toArray
  val groupedTestOutputs = testOutputs.grouped(outputCores).toArray

  poke(c.io.start, false)
  poke(c.io.xInValid, false)

  def vecToBigInt(vec: Seq[Int]): BigInt = {
    int(Bits(vec.reverse.map((n) => n.toString).fold("b")(_ + _)))
  }

  val numTests = testInputs.length
  val defaultPush = 50
  var pushedTests = 0
  var Cycle = 0;

  def Step(n: Int) {
    step(n)
    Cycle += n
    peeks()
  }

  def push(num: Int) {
    groupedTestInputs.slice(pushedTests, pushedTests + num)
      .foreach(groupedTests => {
        for (chunk <- 0 until chunksInImage) {
          for (testI <- 0 until groupedTests.length) {
            val asd = groupedTests(testI)(chunk)
            poke(c.io.xIn(testI), vecToBigInt(asd))
          }
          poke(c.io.xInValid, true)
          Step(1)
          poke(c.io.xInValid, false)
        }
    })
    pushedTests += num
  }

  push(100)

  poke(c.io.start, true)
  Step(1)
  poke(c.io.start, false)

  var stop = false;
  while (!stop && peek(c.io.inputCount) < numTests / inputCores) {
    if (pushedTests - peek(c.io.inputCount) < defaultPush) {
      val toPush = Math.min(defaultPush, numTests - pushedTests)
      push(toPush)
    }
    Step(1000)
    println("pushed %d/%d".format(pushedTests * inputCores, testInputs.length))
    val isDone = peek(c.io.done) == 0x1
    if (isDone) {
      println("WAS DONE INSIDE WHILE LOOP!!")
      stop = true
    }
  }
  poke(c.io.start, false)

  println("ENTERING WHILE NOT DONE LOOP")
  while (peek(c.io.done) == 0x0) { Step(1) }
  for (i <- 0 until (testInputs.length / outputCores)) {
    val solution = vecToBigInt(groupedTestOutputs(i).flatMap(e => e))
    expect(peekAt(c.outputMem, i) == solution,
      "Image #%d (should be %x)".format(i, solution))
  }
  println("Total cycles: %d".format(Cycle))
  println("  %5.2f cycles per image".format(Cycle.toFloat / testInputs.length))
}

object NetSimulation {
  def main(args: Array[String]) {
    val margs = Array("--backend", "c", "--genHarness", "--compile", "--test")

    val testData = Utils.readDumpFile()
    val testInput = testData.vectors.map(_(0)).toArray
    val testOutput = testData.vectors.map(_(4)).toArray
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

    chiselMainTest(margs, () => Module(new NetSimulationHarness(layers, testInput.length / parametersList(0).NumberOfCores, 128))) {
      c => new NetSimulationHarnessTests(c, layers, testInput, testOutput)
    }
  }
}
