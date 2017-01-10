package Pacman

import Chisel._

import fpgatidbits.ocm.DualPortBRAM

class CircularPeekQueue(blockSize: Int, numberOfBlocks: Int, dataWidth: Int) extends Module {

  val addressWidth = (Math.log(blockSize.toDouble * numberOfBlocks) / Math.log(2)).ceil.toInt
  val io = new Bundle {
    val writeEnable = Bool().asInput
    val nextBlock = Bool().asInput
    val input = Bits(width=dataWidth).asInput
    val output = Bits(width=dataWidth).asOutput
  }

  val currentBlockOffset = Module(new AsyncCounter(0, blockSize * numberOfBlocks, blockSize))
  currentBlockOffset.io.enable := io.nextBlock

  val inBlockReadOffset = Module(new CounterWithSyncAndAsyncReset(0, blockSize))
  inBlockReadOffset.io.enable := Bool(true)
  inBlockReadOffset.io.asyncRst := io.nextBlock
  inBlockReadOffset.io.syncRst := inBlockReadOffset.io.value === UInt(blockSize -  1)

  val writeAddr = Reg(init=UInt(blockSize, width=UInt(blockSize * numberOfBlocks).getWidth))
  when (io.writeEnable) {
    when (writeAddr === UInt(blockSize * numberOfBlocks - 1)) {
      writeAddr := UInt(0)
    } .otherwise {
      writeAddr := writeAddr + UInt(1)
    }
  }

  val queue = Module(new DualPortBRAM(addressWidth, dataWidth))
  val inputPort = queue.io.ports(0)
  val outputPort = queue.io.ports(1)

  outputPort.req.addr := currentBlockOffset.io.value + Mux(io.nextBlock, UInt(0), inBlockReadOffset.io.value)
  outputPort.req.writeEn := Bool(false)
  io.output := outputPort.rsp.readData

  inputPort.req.addr      := writeAddr
  inputPort.req.writeEn   := io.writeEnable
  inputPort.req.writeData := io.input
}
