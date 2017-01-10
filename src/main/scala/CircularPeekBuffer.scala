package Pacman

import Chisel._

class CircularPeekBuffer(
  numberOfBlocks: Int,
  wordPerBlock: Int,
  wordWidth: Int
) extends Module {

  val io = new Bundle {
    val wordIn = Decoupled(Bits(width=wordWidth)).flip

    val wordOut = Bits(width=wordWidth).asOutput
    val startOut = Bool().asOutput
    val pipeReady = Bool().asInput
  }

  val queue = Module(new CircularPeekQueue(wordPerBlock, numberOfBlocks, wordWidth))

  val wordCounter = Module(new Counter(0, wordPerBlock))
  val readyBlocks = Module(new UpDownCounter(0, numberOfBlocks))

  val isReady = readyBlocks.io.value =/= UInt(numberOfBlocks - 1)
  val signalReadingInput = io.wordIn.valid && isReady
  val signalLastInputWord = signalReadingInput && wordCounter.io.value === UInt(wordPerBlock - 1)
  val signalNewPeekBlock = io.pipeReady && readyBlocks.io.value =/= UInt(0)

  wordCounter.io.enable := signalReadingInput
  wordCounter.io.rst := wordCounter.io.value === UInt(wordPerBlock - 1)

  readyBlocks.io.up := signalLastInputWord
  readyBlocks.io.down := signalNewPeekBlock

  queue.io.input := io.wordIn.bits
  queue.io.writeEnable := signalReadingInput
  queue.io.nextBlock := signalNewPeekBlock

  io.wordOut := queue.io.output
  io.startOut := Reg(init=Bool(false), next=signalNewPeekBlock)
  io.wordIn.ready := isReady
}
