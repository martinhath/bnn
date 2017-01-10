package Pacman

import Chisel._

class Interleaver(firstLayer: LayerParameters) extends Module {

  val K = firstLayer.K
  val cores = firstLayer.NumberOfCores
  val numberOfBlocks = 3
  val wordPerBlock = firstLayer.MatrixWidth / K

  val io = new Bundle {
    val wordIn = Decoupled(Bits(width=K)).flip

    val interleavedOut = Vec.fill(cores)(Bits(width=K)).asOutput
    val startOut = Bool().asOutput
    val pipeReady = Bool().asInput
  }

  val queues = Array.fill(cores)(Module(new CircularPeekQueue(wordPerBlock, numberOfBlocks, K)))


  val wordCounter = Module(new Counter(0, wordPerBlock))
  val queueCounter = Module(new Counter(0, cores))
  val readyBlocks = Module(new UpDownCounter(0, numberOfBlocks))

  val isReady = readyBlocks.io.value =/= UInt(numberOfBlocks - 1)
  val signalReadingInput = io.wordIn.valid && isReady
  val isLastInputWordInBlock = wordCounter.io.value === UInt(wordPerBlock - 1)
  val signalWritingLastWord = signalReadingInput && isLastInputWordInBlock
  val signalNewPeekBlock = io.pipeReady && readyBlocks.io.value =/= UInt(0)
  val isLastQueue = queueCounter.io.value === UInt(cores - 1)
  val signalWritingLastWordInLastQueue = signalWritingLastWord && isLastQueue

  wordCounter.io.enable := signalReadingInput
  wordCounter.io.rst := isLastInputWordInBlock

  queueCounter.io.enable := signalWritingLastWord
  queueCounter.io.rst := isLastQueue

  readyBlocks.io.up := signalWritingLastWordInLastQueue
  readyBlocks.io.down := signalNewPeekBlock

  // queue.io.input := io.wordIn.bits
  // queue.io.writeEnable := signalReadingInput
  // queue.io.nextBlock := signalNewPeekBlock
  queues.zipWithIndex.foreach
  {
    case (q, i) => {
      q.io.input := io.wordIn.bits
      q.io.writeEnable := (queueCounter.io.value === UInt(i)) && signalReadingInput
      q.io.nextBlock :=
        signalNewPeekBlock
      io.interleavedOut(i) := q.io.output
    }
  }

  // io.wordOut := queue.io.output
  io.startOut := Reg(init=Bool(false), next=signalNewPeekBlock)
  io.wordIn.ready := isReady
}
