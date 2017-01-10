package Pacman

import Chisel._

class MemoryUnit(weightsArray: Array[String], restartDelay : Int) extends Module {

  def resettableCounter(max: UInt, reset: Bool) = {
    val x = Reg(init = UInt(0, max.getWidth))
    x := Mux(reset, UInt(1), x + UInt(1))
    x
  }

  val weightsVec = Vec(weightsArray.map(s => Bits(s)))

  val io = new Bundle {
    val restartIn = Bool().asInput()
    val restartOut = Bool().asOutput()
    val weights = Bits(width = Bits(weightsArray(0)).getWidth).asOutput()
  }

  val restartRegs = Vec.fill(restartDelay)(Reg(init = Bool(false)))
  restartRegs.zip(restartRegs.drop(1)).foreach({
                                                 case (a, b) => {
                                                   b := a
                                                 }
                                               })
  restartRegs(0) := io.restartIn
  io.restartOut := restartRegs.last

  val counter = resettableCounter(UInt(weightsVec.length), io.restartIn)

  val addr = Mux(io.restartIn, UInt(0), counter)
  val weightsReg = Reg(init = Bits(0))
  weightsReg := weightsVec(addr)
  io.weights := weightsReg
}
