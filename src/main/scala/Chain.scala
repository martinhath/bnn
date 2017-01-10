package Pacman

import Chisel._

class Chain(parameters: LayerParameters) extends Module {

  val processingUnits =
    List.fill(parameters.NumberOfPUs)(Module(new ProcessingUnit(parameters)))

  val io = new Bundle {
    val weights =
      Vec.fill(parameters.NumberOfPUs) { Bits(width = parameters.K) }.asInput
    val bias = UInt(width = parameters.BiasWidth).asInput
    val restartIn = Bool().asInput
    val xs = Bits(width = parameters.K).asInput
    val ys = Vec
      .fill(parameters.NumberOfPUs) {
        Bits(width = parameters.AccumulatorWidth)
      }
      .asOutput
  }

  for (i <- 0 until parameters.NumberOfPUs) {
    io.ys(i) := processingUnits(i).io.yOut
    processingUnits(i).io.bias := io.bias
    processingUnits(i).io.ws := io.weights(i)
  }

  processingUnits
    .zip(processingUnits.drop(1))
    .foreach({
      case (a, b) => {
        b.io.restartIn := a.io.restartOut
        b.io.xs := a.io.xOut
      }
    })

  processingUnits(0).io.xs := io.xs
  processingUnits(0).io.restartIn := io.restartIn
}
