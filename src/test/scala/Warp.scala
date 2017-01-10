package Pacman

import util.Random
import scala.collection.mutable.PriorityQueue

import Chisel._

class WarpTests(c: Warp,
                p: LayerParameters,
                weights: Array[Array[Int]],
                biases: Array[Int]
  ) extends Tester(c) {
  def vecToBigInt(vec: Seq[Int]): BigInt = {
    int(Bits(vec.reverse.map((n) => n.toString).fold("b")(_ + _)))
  }

  if (p.NumberOfCores == 0)
    throw new AssertionError("Warp need to have NumberOfCores set")
  if (p.MatrixHeight % p.NumberOfPUs != 0)
    throw new AssertionError("NumberOfPUs needs to divide MatrixHeight")

  val Iters = 8
  val xs = List.fill(Iters) {
            List.fill(p.NumberOfCores) {
              List.fill(p.MatrixWidth) {
                Random.nextInt(2) }}}

  val expectedResults = xs.map(e => e.map(vec =>
      Matrix.matrixMul(weights, vec)
        .zip(biases)
        .map({ case (dot, bias) => {
                val a = dot * 2 - p.MatrixWidth + (Math.floor(bias / 2.0).toInt * 2)
          if (a >= 0) 1 else 0
        }})))

  val passesRequired = p.MatrixHeight / p.NumberOfPUs
  val cyclesPerPass = p.MatrixWidth / p.K
  val PUsPerMUs = p.NumberOfPUs / p.NumberOfMS

  val waitingSteps = List(0, 1, cyclesPerPass / 2, 3, 2, 6, 0, 0, 0)

  var Cycle = 0

  var expectQueue: PriorityQueue[(Int, () => Unit)] =
    PriorityQueue()(Ordering.by(e => -e._1))
  def handleQueue(cycle: Int) {
    if (expectQueue.length == 0)
      return
    var top = expectQueue.minBy(e => e._1)
    if (top._1 == cycle) {
      top._2.apply
      expectQueue.dequeue
      handleQueue(cycle)
    }
  }
  var shouldBeReady = false

  Matrix.printMat(weights)
  Matrix.printMat(Array(biases))

  for (i <- 0 until Iters) {
    for (j <- 0 until p.NumberOfCores) {
      Matrix.printMat(Array(xs(i)(j).toArray))
      Matrix.printMat(expectedResults(i).map(_.toArray).toArray)
    }
  }

  step(100)

  poke(c.io.pipeReady, true)
  for (iter <- 0 until Iters) {
    poke(c.io.start, true)
    expect(c.io.ready, false)
    val nextReadyCycle = Cycle + passesRequired * cyclesPerPass + PUsPerMUs - 2
    expectQueue.enqueue((nextReadyCycle, () => {
      expect(c.io.ready, true)
      shouldBeReady = true
    }))
    for (pass <- 0 until passesRequired) {
      for (i <- 0 until cyclesPerPass) {
        if ((pass != 0 || i != 0) &&
            (pass != passesRequired - 1 || i !=  cyclesPerPass - 1)) {
          expect(c.io.ready, false)
          poke(c.io.start, false)
        }
        for (core <- 0 until p.NumberOfCores) {
          val lower = i * p.K
          val upper = (i + 1) * p.K
          poke(c.io.xIn(core), vecToBigInt(xs(iter)(core).slice(lower, upper)))

          if (i < p.NumberOfPUs) {
            val _i = i
            val _core = core
            val _iter = iter
            val _pass = pass
            expectQueue.enqueue((Cycle + cyclesPerPass, () => {
                expect(c.io.xOutValid, 1)
                val ind = _i + p.NumberOfPUs * _pass
                expect(c.io.xOut(_core), expectedResults(_iter)(_core)(ind))
                if (_i == p.NumberOfPUs - 1 && _pass == passesRequired - 1) {
                  expect(c.io.done, true)
                }
              }))
          }
        }
        step(1)
        Cycle += 1
        handleQueue(Cycle)
      }
    }
    while (Cycle < nextReadyCycle) {
      expect(c.io.ready, false)
      step(1)
      Cycle += 1
      handleQueue(Cycle)
    }
    expect(c.io.ready, true)
    step(1)
    Cycle += 1
    handleQueue(Cycle)

    for (i <- 0 until waitingSteps(iter)) {
      step(1)
      Cycle += 1
      handleQueue(Cycle)
      poke(c.io.pipeReady, false)
      expect(c.io.ready, false)
      poke(c.io.pipeReady, true)
      expect(c.io.ready, shouldBeReady)
    }
    shouldBeReady = false
  }
}

class WarpControlTests1(c: WarpControl, p: LayerParameters) extends Tester(c) {

  def peekAll() {
    peek(c.io)
    peek(c.isActive.io)
    peek(c.isReady.io)
    peek(c.isOutputting.io)
    peek(c.cycleInPass.io)
    peek(c.cycle.io)
    peek(c.tailCycle.io)
    peek(c.selectX.io)
  }


  poke(c.io.start, false)
  poke(c.io.nextReady, true)

  peekAll()
  step(100)
  peekAll()


  expect(c.io.ready, true)
  expect(c.io.valid, false)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, true)
  expect(c.io.chainRestart, true)

  poke(c.io.start, true)
  step(1)

  expect(c.io.ready, false)
  expect(c.io.valid, false)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, false)
  expect(c.io.chainRestart, false)

  poke(c.io.start, false)
  step(1)

  List.range(0, 10).foreach(i => {
    expect(c.io.ready, false)
    expect(c.io.valid, false)
    expect(c.io.done, false)
    expect(c.io.memoryRestart, i == 9)
    expect(c.io.chainRestart, false)
    step(1)
  })

  expect(c.io.ready, false)
  expect(c.io.valid, true)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, false)
  expect(c.io.chainRestart, true)
  step(1)

  List.range(0, 5).foreach(_ => {
    expect(c.io.ready, false)
    expect(c.io.valid, true)
    expect(c.io.done, false)
    expect(c.io.memoryRestart, false)
    expect(c.io.chainRestart, true)
  peekAll()
    step(1)
  })

  expect(c.io.ready, true)
  expect(c.io.valid, true)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, true)
  expect(c.io.chainRestart, true)

  peekAll()
  step(1)

  expect(c.io.ready, true)
  expect(c.io.valid, true)
  expect(c.io.done, true)
  expect(c.io.memoryRestart, true)
  expect(c.io.chainRestart, true)

  peekAll()
  step(1)
  step(1)
  step(1)

  poke(c.io.start, true)
  expect(c.io.memoryRestart, false)
  step(1)

  expect(c.io.ready, false)
  expect(c.io.valid, false)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, false)
  expect(c.io.chainRestart, false)
}

class WarpControlTests2(c: WarpControl, p: LayerParameters) extends Tester(c) {

  def peekAll() {
    peek(c.io)
    peek(c.isActive.io)
    peek(c.isReady.io)
    peek(c.isOutputting.io)
    peek(c.cycleInPass.io)
    peek(c.cycle.io)
    peek(c.tailCycle.io)
    peek(c.selectX.io)
  }

  poke(c.io.start, false)
  poke(c.io.nextReady, true)

  step(100)

  expect(c.io.ready, true)
  expect(c.io.valid, false)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, true)
  expect(c.io.chainRestart, true)

  poke(c.io.start, true)
  expect(c.io.memoryRestart, false)
  peekAll()
  step(1)
  peekAll()
  poke(c.io.start, false)

  List.range(0, 5).foreach(_ => {
    expect(c.io.ready, false)
    expect(c.io.valid, false)
    expect(c.io.done, false)
    expect(c.io.memoryRestart, false)
    expect(c.io.chainRestart, false)

    step(1)
  })

  expect(c.io.ready, false)
  expect(c.io.valid, true)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, false)
  expect(c.io.chainRestart, true)

  step(1)

  // Get out the four first ys
  List.range(0, 3).foreach(_ => {
    expect(c.io.ready, false)
    expect(c.io.valid, true)
    expect(c.io.done, false)
    expect(c.io.memoryRestart, false)
    expect(c.io.chainRestart, false)

    step(1)
  })

  expect(c.io.ready, false)
  expect(c.io.valid, false)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, false)
  expect(c.io.chainRestart, false)

  step(1)

  expect(c.io.ready, false)
  expect(c.io.valid, false)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, true)
  expect(c.io.chainRestart, false)

  step(1)

  // 5th Y     112
  expect(c.io.ready, true)
  expect(c.io.valid, true)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, true)
  expect(c.io.chainRestart, true)

  step(1)
  poke(c.io.start, true)

  // 6th Y     113
  expect(c.io.ready, true)
  expect(c.io.valid, true)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, false)
  expect(c.io.chainRestart, true)

  step(1)
  poke(c.io.start, false)

  // 7th Y     114
  expect(c.io.ready, false)
  expect(c.io.valid, true)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, false)
  expect(c.io.chainRestart, false)

  step(1)
//  peek(c.cycleInPassCounter.io)
//  peek(c.totalCycleCounter.io)
//  peek(c.selectXCounter.io)

  // 8th Y     115
  expect(c.io.ready, false)
  expect(c.io.valid, true)
  expect(c.io.done, true)
  expect(c.io.memoryRestart, false)
  expect(c.io.chainRestart, false)

  step(1)
}

class WarpControlTests3(c: WarpControl, p: LayerParameters) extends Tester(c) {
  poke(c.io.start, false)
  poke(c.io.nextReady, true)
//  peek(c.outputCounter.io)
  step(100)
//  peek(c.outputCounter.io)

  expect(c.io.ready, true)
  expect(c.io.valid, false)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, true)
  expect(c.io.chainRestart, true)

  poke(c.io.start, true)
  expect(c.io.memoryRestart, false)
  step(1)
  poke(c.io.start, false)

  expect(c.io.ready, false)
  expect(c.io.valid, false)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, false)
  expect(c.io.chainRestart, false)

  step(1)

  List.range(0, 4).foreach(_ => {
    expect(c.io.ready, false)
    expect(c.io.valid, false)
    expect(c.io.done, false)
    expect(c.io.memoryRestart, false)
    expect(c.io.chainRestart, false)

    step(1)
  })

  List.range(0, 4).foreach(i => {
    expect(c.io.ready, false)
    expect(c.io.valid, true)
    expect(c.io.done, false)
    expect(c.io.memoryRestart, false)
    expect(c.io.chainRestart, i == 0)

    step(1)
  })

  // 110
  expect(c.io.ready, false)
  expect(c.io.valid, false)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, false)
  expect(c.io.chainRestart, false)

//  peek(c.cycleInPassCounter.io)
  step(1)

  // 111
  expect(c.io.ready, true)
  expect(c.io.valid, false)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, true)
  expect(c.io.chainRestart, false)

//  peek(c.totalCycleCounter.io)
//  peek(c.cycleInPassCounter.io)
  step(1)

  // 112
  expect(c.io.ready, true)
  expect(c.io.valid, true)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, true)
  expect(c.io.chainRestart, true)

//  peek(c.totalCycleCounter.io)
//  peek(c.cycleInPassCounter.io)

  poke(c.io.start, true)
  expect(c.io.memoryRestart, false)
  expect(c.io.chainRestart, true)

  step(1)
  poke(c.io.start, false)

//  peek(c.cycleInPassCounter.io)

  // 113
  expect(c.io.ready, false)
  expect(c.io.valid, true)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, false)
  expect(c.io.chainRestart, false)

  step(1)

  // 114
  expect(c.io.ready, false)
  expect(c.io.valid, true)
  expect(c.io.done, false)
  expect(c.io.memoryRestart, false)
  expect(c.io.chainRestart, false)

  step(1)

  // 115
  expect(c.io.ready, false)
  expect(c.io.valid, true)
  expect(c.io.done, true)
  expect(c.io.memoryRestart, false)
  expect(c.io.chainRestart, false)
}

object WarpTest {
  def main(args: Array[String]) {
    val margs = Array("--backend", "c", "--genHarness", "--compile", "--test")

    // val p1 = new LayerParameters(
    //   K=1,
    //   BiasWidth=8,
    //   AccumulatorWidth=10,
    //   NumberOfPUs=8,
    //   NumberOfMS=1,
    //   MatrixWidth=12,
    //   MatrixHeight=8,
    //   NumberOfCores=3
    //   )
    // chiselMainTest(margs, () => Module(new WarpControl(p1))) {
    //   c => new WarpControlTests1(c, p1)
    // }

    // val p2 = new LayerParameters(
    //   K=2,
    //   BiasWidth=8,
    //   AccumulatorWidth=10,
    //   NumberOfPUs=4,
    //   NumberOfMS=2,
    //   MatrixWidth=12,
    //   MatrixHeight=8,
    //   NumberOfCores=3
    //   )
    // chiselMainTest(margs, () => Module(new WarpControl(p2))) {
    //   c => new WarpControlTests2(c, p2)
    // }

    // val p3 = new LayerParameters(
    //   K=2,
    //   BiasWidth=8,
    //   AccumulatorWidth=10,
    //   NumberOfPUs=4,
    //   NumberOfMS=4,
    //   MatrixWidth=12,
    //   MatrixHeight=8,
    //   NumberOfCores=3
    //   )
    // chiselMainTest(margs, () => Module(new WarpControl(p3))) {
    //   c => new WarpControlTests3(c, p3)
    // }

    Random.setSeed(12)
    // val p = new LayerParameters(
    //   K=16,
    //   BiasWidth=8,
    //   AccumulatorWidth=10,
    //   NumberOfPUs=32,
    //   NumberOfMS=8,
    //   MatrixWidth=784,
    //   MatrixHeight=256,
    //   NumberOfCores=4
    //   )

    // val weights = Array.fill(p.MatrixHeight) {
    //   Array.fill(p.MatrixWidth) {
    //     Random.nextInt(2)
    //   }
    // }
    // val biases = Array.fill(p.MatrixHeight) {
    //   (Random.nextInt(Math.pow(2, p.BiasWidth).toInt)
    //    - Math.pow(2, p.BiasWidth - 1)).toInt
    // }

    // chiselMainTest(margs, () => Module(new Warp(p, weights, biases))) {
    //   c => new WarpTests(c, p, weights, biases)
    // }

    // return

    def divisors(n: Int): Seq[Int] = {
      List.range(1, n + 1).filter((e) => n % e == 0)
    }

    for (matHeight <- List(2, 4, 8, 10, 32)) {
      for (matWidth <- List(3, 8, 24, 32)) {
        val possibleKs = divisors(matWidth)
        for (k <- possibleKs) {
          val possiblePUs = List.range(1, Math.min(matHeight, matWidth / k)).filter(pus => matHeight % pus == 0)
          for (pus <- possiblePUs) {
            val possibleMUs = divisors(pus)
            for (mus <- possibleMUs) {
              for (cores <- List(1, 2, 3)) {
                val params = new LayerParameters(
                  K = k,
                  BiasWidth = 8,
                  AccumulatorWidth = 10,
                  NumberOfPUs = pus,
                  NumberOfCores = cores,
                  NumberOfMS = mus,
                  MatrixHeight = matHeight,
                  MatrixWidth = matWidth
                )

                val weights = Array.fill(matHeight) {
                  Array.fill(matWidth) {
                    Random.nextInt(2)
                  }
                }
                val biases = Array.fill(matHeight) {
                  (Random.nextInt(Math.pow(2, params.BiasWidth).toInt)
                   - Math.pow(2, params.BiasWidth - 1)).toInt
                }

                println(params)
                val layerData = new LayerData(params, weights, biases)
                chiselMainTest(margs, () => Module(new Warp(layerData))) {
                  c => new WarpTests(c, params, weights, biases)
                }
              }
            }
          }
        }
      }
    }


  }
}
