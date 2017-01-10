package Pacman

import Chisel._

import fpgatidbits.ocm.DualPortBRAM

class GearBox(p: GearBoxParameters) extends Module {
  if (p.Previous.MatrixHeight == 0)
    throw new AssertionError("p.Previous.MatrixHeight needs to be set")
  if (p.Previous.NumberOfCores == 0)
    throw new AssertionError("p.Previous.NumberOfCores needs to be set")
  if (p.Next.NumberOfCores == 0)
    throw new AssertionError("p.Next.NumberOfCores needs to be set")
  if (p.Next.K == 0) throw new AssertionError("p.Next.K needs to be set")

  val blocksPerQueue = 3
  val wordsPerBlock = p.Previous.MatrixHeight / p.Next.K
  val numberOfQueues = Math.max(p.Previous.NumberOfCores, p.Next.NumberOfCores)
  val maxReadyBlocks = numberOfQueues * blocksPerQueue - p.Next.NumberOfCores
  // if (numberOfQueues != 1) {
  //   throw new AssertionError("GearBox does not support multiple cores yet!")
  // }

  val io = new Bundle {
    val xsIn = Vec.fill(p.Previous.NumberOfCores) { Bits(width = 1) }.asInput
    val validIn = Bool().asInput
    val prevDone = Bool().asInput
    val prevStart = Bool().asInput
    val ready = Bool().asOutput

    val nextReady = Bool().asInput
    val startNext = Bool().asOutput
    val xsOut = Vec.fill(p.Next.NumberOfCores) { Bits(width = p.Next.K) }.asOutput
  }

  // Make bitBuffers
  val bitBuffers = Array.fill(p.Previous.NumberOfCores)(Module(new BitToWord(p.Next.K)))
  val bitBufferWords = Vec(bitBuffers.map(_.io.word))
  val fillingBlock = Module(new AsyncUpDownCounter(0, 2*p.Previous.NumberOfCores, p.Previous.NumberOfCores))
  fillingBlock.io.up    := io.prevStart
  fillingBlock.io.down  := Reg(init=Bool(false), next=io.prevDone)

  val bitCounter = Module(new Counter(0, p.Next.K))
  val blocksReady = Module(new UpDownCounter(0, maxReadyBlocks, p.Previous.NumberOfCores, p.Next.NumberOfCores))

  val signalResetBitBuffers = bitCounter.io.value === UInt(p.Next.K - 1)
  val signalBitBuffersFull = Reg(init=Bool(false), next=io.validIn && signalResetBitBuffers)
  val signalNewPeekBlock = io.nextReady && blocksReady.io.value >= UInt(p.Next.NumberOfCores)


  // Count how many ready blocks we have in the queue
  blocksReady.io.up   := Reg(init=Bool(false), next=io.prevDone)
  blocksReady.io.down := signalNewPeekBlock

  val reservedBlocks = blocksReady.io.value + fillingBlock.io.value
  val hasEnoughEmptyBlocks = reservedBlocks <= UInt(maxReadyBlocks - p.Previous.NumberOfCores)

  io.ready := hasEnoughEmptyBlocks
  io.startNext := Reg(init=Bool(false), next=signalNewPeekBlock)

  // Hook up bitBuffers to input so they get filled up
  bitBuffers.zip(io.xsIn).foreach {
    case (b, x) => {
      b.io.enable := io.validIn
      b.io.bit := x
    }
  }

  // Count how many bits are in the bitBuffers
  bitCounter.io.enable := io.validIn
  bitCounter.io.rst := signalResetBitBuffers

  // Make queues
  val queues = Array.fill(numberOfQueues)(
    Module(new CircularPeekQueue(wordsPerBlock, blocksPerQueue, p.Next.K))
  )
  val queueOutputs = Vec(queues.map(_.io.output))

  // Make inputSelectCounters - queues muxes over input
  val inputSelectCounters = Array.range(0, numberOfQueues)
    .map(i => Module(new WrappingCounter(i, numberOfQueues,
                                         numberOfQueues - p.Previous.NumberOfCores)))
    .toArray

  inputSelectCounters.foreach(c => {
                                c.io.enable := Reg(init=Bool(false), next=io.prevDone)
                                c.setName("inputSelectCounter")
                              })

  // Not every queue can take input if there are more queues than cores in Next
  val inputEnables = Vec(
    Array.fill(p.Previous.NumberOfCores)(Bool(true))
      ++ Array.fill(numberOfQueues - p.Previous.NumberOfCores)(Bool(false))
  )

  // Hook up queues to input
  queues.zip(inputSelectCounters).foreach{
    case(q, c) => {
      q.io.writeEnable := signalBitBuffersFull && inputEnables(c.io.value)
      q.io.input := bitBufferWords(c.io.value)
    }
  }

  // Make queueOutputSelectCounters -  outputs muxes over queues
  val queueOutputSelectCounters = Array.range(0, p.Next.NumberOfCores)
    .map(i => Module(new WrappingCounter(i + numberOfQueues - p.Next.NumberOfCores, numberOfQueues, p.Next.NumberOfCores)))
    .toArray

  queueOutputSelectCounters.foreach(c => {
                                      c.setName("queueOutputSelectCounters")
                                      c.io.enable := signalNewPeekBlock
                                    })

  // Hook up output to queues
  io.xsOut.zip(queueOutputSelectCounters).foreach{
    case(xs, c) => {
      xs := queueOutputs(c.io.value)
    }
  }

  // Make outputSelectCounters - queues muxes over outputs
  val outputSelectCounters = Array.range(0, numberOfQueues)
    .map(i => Module(new WrappingCounter(i, numberOfQueues, numberOfQueues - p.Next.NumberOfCores)))
    .toArray

  outputSelectCounters.foreach(c => {
                                 c.setName("outputSelectCounters")
                                 c.io.enable := signalNewPeekBlock
                               })

  val outputEnables = Vec(
    Array.fill(p.Next.NumberOfCores)(Bool(true))
      ++ Array.fill(numberOfQueues - p.Next.NumberOfCores)(Bool(false))
  )

  queues.zip(outputSelectCounters).foreach{
    case(q, c) => {
      q.io.nextBlock := signalNewPeekBlock && outputEnables(c.io.value)
    }
  }
}
