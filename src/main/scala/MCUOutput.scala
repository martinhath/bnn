package Pacman

//import fpgatidbits.ocm.DualPortBRAM
import scala.language.reflectiveCalls
import Chisel._



class MCUOutput(width: Int ) extends Module {
  val io = new Bundle {
    val oneHotIn = Decoupled(Bits(log2Ceil(width))).flip
    val ebi = Decoupled(Bits(width=16))
    val ebiAck = Bool().asInput
  }
  val outBuff = Vec.fill(4) { Reg(init=Bits(0,width=4)) }
//  val fifo = Module( new DualPortBRAM(width,log2Ceil(width)))
  val fifo = Mem(128 ,Bits(width=log2Ceil(width)))
  val outCtrl = Module(new MCUOutCtrl)
  val ackTgl = Reg(init=Bool(false))
  val ebiRdy = Reg(next=io.ebi.ready)
  val ebiAck = Reg(next=io.ebiAck)
  val rdyTgl = Reg(init=Bool(false))
  if (log2Ceil(width) != 4) { throw new AssertionError("Wrong Matrix height.") }

  // Output 
  io.ebi.bits := Cat(outBuff(0), outBuff(1), outBuff(2), outBuff(3))
  io.oneHotIn.ready := outCtrl.io.addr =/= UInt(127)
  io.ebi.valid := Mux(ebiRdy, outCtrl.io.validOut,Bool(false))
  
  // Control Input
  ackTgl := ebiAck
  rdyTgl := ebiRdy
  outCtrl.io.readyLow := rdyTgl && ~ebiRdy
  outCtrl.io.validIn := io.oneHotIn.valid
  outCtrl.io.fillIn := ~ackTgl && ebiAck
   
  //Set up fifo buffer write and read ports 
  when(io.oneHotIn.valid){
    fifo(0) := io.oneHotIn.bits 
    for(i <- 1 until 128) {
      fifo(i) := fifo(i-1)
    }
  }
//  fifo.io.ports(0).req.writeEn := io.oneHotIn.valid
//  fifo.io.ports(0).req.addr := outCtrl.io.addrIn 
//  fifo.io.ports(0).req.writeData := io.oneHotIn.bits

//  fifo.io.ports(1).req.addr := outCtrl.io.addrOut
//  fifo.io.ports(1).req.writeEn := Bool(false)
  // Fill out buffer when in the right state
  when(outCtrl.io.state(0)===Bits(1)) { outBuff(0) := fifo(outCtrl.io.addr)}//fifo.io.ports(1).rsp.readData }
  when(outCtrl.io.state(1)===Bits(1)) { outBuff(1) := fifo(outCtrl.io.addr)}//fifo.io.ports(1).rsp.readData }
  when(outCtrl.io.state(2)===Bits(1)) { outBuff(2) := fifo(outCtrl.io.addr)}//fifo.io.ports(1).rsp.readData }
  when(outCtrl.io.state(3)===Bits(1)) { outBuff(3) := fifo(outCtrl.io.addr)}//fifo.io.ports(1).rsp.readData }
  
}

