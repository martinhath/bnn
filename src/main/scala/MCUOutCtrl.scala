package Pacman

import scala.language.reflectiveCalls
import Chisel._

class MCUOutCtrl extends Module {
  val io = new Bundle{
    val fillIn = Bool().asInput
    val validIn = Bool().asInput 
    val validOut = Bool().asOutput
    val readyLow = Bool().asInput
    val state = Vec.fill(5) { Bits(width=1) }.asOutput
    val addr = UInt(width=5).asOutput
//    val addrIn = UInt(width=10).asOutput
//    val addrOut = UInt(width=10).asOutput
//    val offset = UInt(width=10).asOutput
  }
  val fifoAddr = Reg(init=UInt(0,width=5))
//  val fifoAddrIn = Reg(init=UInt(0,width=10))
//  val fifoAddrOut = Reg(init=UInt(0,width=10))
  val fillToggle = Reg(init=Bool(false))
  val fill = Reg(init=Bool(false))
  val fillState = Vec(
    Reg(init=Bits(0)), // Fill reg 0
    Reg(init=Bits(0)), // Fill reg 1
    Reg(init=Bits(0)), // Fill reg 2
    Reg(init=Bits(0)), // Fill reg 3
    Reg(init=Bits(1))  // Idle state
  ) 
  val valid = Reg(init=Bool(false))
//  val offset = fifoAddrIn - fifoAddrOut
 
  // Outputs
  io.state := fillState
  io.addr := fifoAddr
//  io.addrIn := fifoAddrIn
//  io.addrOut := fifoAddrOut
  io.validOut := valid
//  io.offset := offset

  //valid is recieved and ready went low, setting valid low
  when (valid && io.readyLow){
    valid := ~valid 
  }

  // The fifo gets 1 valid number
  when (io.validIn && ~fill ) {
    fifoAddr := fifoAddr + UInt(1)
  }

  // Data is available and the states will cycle once
  when ((fifoAddr >= UInt(5)) && fillToggle && ~valid){
    fill := ~fill
    fillToggle := ~fillToggle
  }

  // Toggle that the MCU has recieved data, and can replace data on the bus
  when (io.fillIn &&  ~fillToggle ){
    fillToggle := ~fillToggle
  }

  when(fill) { 
    // Cycling of the one-hot value that spacifies the state
    fillState(0) := fillState(4)
    for(i <- 1 until 5){
      fillState(i) := fillState(i-1)
    }
    //when(~(fillState(3)===Bits(1))) {
    //fifoAddrOut := fifoAddrOut + UInt(1)
    //}
    when(fillState(3)===Bits(1)){ 
      fill := ~fill // resets when the state machine reaches Idle
      valid := ~valid
    } .elsewhen (~io.validIn){
      fifoAddr := fifoAddr - UInt(1)
    } 

  }
}
