package Pacman

import Chisel._

// The Activation box takes a vector of the bitsum from Chain.
// This sum isn't really what we want, as the _actual_ parameters
// are -1 and 1, not 0 and 1. Therefore, we have to multiply
// by two, and subtract the width of the matrix.
// Then, as the biases are already addded into the product,
// we just need to compare with zero, which is the upper bit.
class Activation(parameters: LayerParameters) extends Module {
  // TODO: Add rest of used parameters?
  if (parameters.MatrixWidth == 0) {
    throw new AssertionError("Activation needs parameters.MatrixWidth to be set")
  }

  val io = new Bundle {
    val in = Vec.fill(parameters.NumberOfPUs){
      SInt(width = parameters.AccumulatorWidth)
    }.asInput
    val out = Vec.fill(parameters.NumberOfPUs){ Bits(width=1) }.asOutput
  }

  for (i <- 0 until parameters.NumberOfPUs) {
    val value = io.in(i) * SInt(2) - SInt(x=parameters.MatrixWidth)
    // Output is the upper bit
    io.out(i) := ~value(parameters.AccumulatorWidth-1)
  }
}
