package Pacman

import Chisel._

/**
  * A ProcessingUnit is a block which "runs"
  * along a single row in the matrix, while
  *  calculating the inner product.
  *
  * The width `k` of a runner is how many bits
  * of the input vector it takes each cycle
  */
class ProcessingUnit(parameters: LayerParameters) extends Module {
  val yReg = Reg(SInt(width = parameters.AccumulatorWidth), init = SInt(0))
  val xReg = Reg(Bits(width = parameters.K), init = Bits(0))
  val restartReg = Reg(Bool(), init = Bool(false))

  val io = new Bundle {
    val xs = Bits(width = parameters.K).asInput
    val ws = Bits(width = parameters.K).asInput
    val xOut = Bits(width = parameters.K).asOutput
    val yOut = SInt(width = parameters.AccumulatorWidth).asOutput
    val bias = SInt(width = parameters.BiasWidth).asInput
    val restartIn = Bool().asInput
    val restartOut = Bool().asOutput
  }

  val innerProd = PopCount(~(io.xs ^ io.ws))
  yReg := Mux(io.restartIn, innerProd + io.bias, innerProd + yReg)

  restartReg := io.restartIn
  io.restartOut := restartReg

  xReg := io.xs
  io.yOut := yReg
  io.xOut := xReg
}
