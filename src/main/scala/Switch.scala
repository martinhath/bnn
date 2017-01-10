package Pacman

import Chisel._

class Switch(init: Boolean = false) extends Module {
  val stateReg = Reg(init=Bool(init))
  val io = new Bundle {
    val signalOn = Bool().asInput
    val state = Bool().asOutput
    val rst = Bool().asInput
  }

  when (io.signalOn) {
    stateReg := Bool(true)
  } .elsewhen (io.rst) {
    stateReg := Bool(false)
  } .otherwise {
    stateReg := stateReg
  }
  io.state := stateReg || io.signalOn
}

