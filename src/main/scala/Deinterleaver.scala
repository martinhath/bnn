package Pacman

import scala.language.reflectiveCalls
import Chisel._

class Deinterleaver(parameters: LayerParameters) extends Module {
  val io = new Bundle {
    val oneBitPerCore = Decoupled(Bits(width=parameters.NumberOfCores )).flip
    val oneHotOut = Decoupled(Bits(OUTPUT,width=parameters.MatrixHeight ))
    val doneIn = Bool(INPUT)
  }
  val regs = List.fill(parameters.NumberOfCores)(Module(new BitToWord(parameters.MatrixHeight )))
  val regsBuf = Vec.fill(parameters.NumberOfCores)(Reg(init=Bits(0,width=parameters.MatrixHeight )))
  val doneDelay = Reg(init=Bool(false))
  val send = Reg(init=Bool(false))
  val count = Reg(init=UInt(parameters.NumberOfCores-1,width=log2Up(parameters.NumberOfCores+1)))
  if (parameters.NumberOfCores != 1){
    when(send && io.oneHotOut.ready){
      count :=count-UInt(1)
    }
  } 
  for ( i <- 0 until parameters.NumberOfCores) {
    regs(i).io.bit := io.oneBitPerCore.bits(i)
    regs(i).io.enable := io.oneBitPerCore.valid
    when (doneDelay) {
      regsBuf(i) := regs(i).io.word
    } 
  }
  doneDelay := io.doneIn
  when(doneDelay || count===UInt(0)) {
    send := doneDelay
  }
  when(send && count===UInt(0)){
   count := UInt(parameters.NumberOfCores-1)
  }
  io.oneBitPerCore.ready := io.oneHotOut.ready
  io.oneHotOut.bits := regsBuf(count)
  io.oneHotOut.valid := send
}


