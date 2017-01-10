package Pacman

import Chisel._

class MemoryStreamer(
  parameters: LayerParameters,
  weightStreams: Array[Array[String]],
  biasStream: Array[String]) extends Module {

  val PUsPerMU = parameters.NumberOfPUs / parameters.NumberOfMS

  // Assertions
  if (weightStreams.exists(a => a.exists(s => s.length() != PUsPerMU * parameters.K + 1))) {
    throw new AssertionError("Weights should be consistent with expected width")
  }
  if (biasStream.exists(s => s.length() != parameters.BiasWidth + 1)) {
    throw new AssertionError("Biases should be consistent with BiasWidth.")
  }

  val weightMemoryUnits = Range(0, parameters.NumberOfMS)
    .map(i => Module(new MemoryUnit(weightStreams(i), PUsPerMU)))
  val biasMemoryUnit = Module(new MemoryUnit(biasStream, PUsPerMU))

  val io = new Bundle {
    val restart = Bool().asInput()
    val weights = Vec
      .fill(parameters.NumberOfPUs) {
        Bits(width = parameters.K)
      }
      .asOutput()
    val bias = SInt(width = parameters.BiasWidth).asOutput()
  }

  // Chain the weight MUs
  weightMemoryUnits
    .zip(weightMemoryUnits.drop(1))
    .foreach({
      case (a, b) => {
        b.io.restartIn := a.io.restartOut
      }
    })
  weightMemoryUnits(0).io.restartIn := io.restart

  // Map bits from MUs to PUs
  Range(0, parameters.NumberOfPUs)
    .zip(weightMemoryUnits.flatMap(List.fill(PUsPerMU)(_)))
    .foreach({
      case (i, mu) => {
        val muOffset = parameters.K * (i % PUsPerMU)
        io.weights(i) := mu.io.weights(muOffset + parameters.K - 1, muOffset)
      }
    })


  // Bias
  biasMemoryUnit.io.restartIn := io.restart
  io.bias := biasMemoryUnit.io.weights
}
