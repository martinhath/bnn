package Pacman

import Chisel._

class PacmanSpeedTestHarness(
  layers: List[LayerData],
  inData: Array[Array[Int]],
  outData: Array[Int],
  inputLimit: Int
) extends Module {
  def arrayToBits(arr: Array[Int]): Bits = {
    Bits(arr.reverse.map((n) => n.toString).fold("b")(_ + _), width=arr.length)
  }

  val inputWidth = inData(0).length
  val outputWidth = log2Up(layers.last.parameters.MatrixHeight)

  val io = new Bundle {
    val leds = Vec.fill(8)(Bool()).asOutput
  }

  val firstCycle = Reg(init=Bool(true), next=Bool(false))

  val pacman = Module(new Pacman(inputWidth, layers))
  pacman.io.digitOut.ready := Bool(true)
  pacman.io.inDataStream.valid := !firstCycle

  val inputCounter = Module(new AsyncCounter(0, inData.length))
  inputCounter.io.enable := pacman.io.inDataStream.ready && !firstCycle

  val inMem = Vec(inData.map(arrayToBits).toArray)
  val inReg = Reg(
    init=Bits(0, width=inputWidth),
    next=inMem(inputCounter.io.value)
  )

  pacman.io.inDataStream.bits := inReg

  val outputCounter = Module(new AsyncCounter(0, outData.length))
  outputCounter.io.enable := pacman.io.digitOut.valid

  val outMem = Vec(outData.map(UInt(_, width=outputWidth)).toArray)
  val outReg = Reg(
    init=Bits(0, width=outputWidth),
    next=outMem(outputCounter.io.value)
  )


  val resultReg = Reg(init=UInt(outData(0), width=outputWidth))
  val correctResultReg = Reg(init=UInt(outData(0), width=outputWidth))
  when(pacman.io.digitOut.valid) {
    resultReg := pacman.io.digitOut.bits
    correctResultReg := outReg
  }.otherwise {
    resultReg := resultReg
    correctResultReg := correctResultReg
  }

  val currentResultCorrect = resultReg === correctResultReg
  val incorrectSwitch = Module(new Switch())
  incorrectSwitch.io.signalOn := !currentResultCorrect
  incorrectSwitch.io.rst := Bool(false)

  val totalCounterWidth = Math.max(6, log2Up(inputLimit+1))
  val totalCounterEnd = Math.max(Math.pow(2, 6).toInt + 1, inputLimit+1)
  val totalCounter = Module(new Counter(0, totalCounterEnd))
  val doneWithAllInput = totalCounter.io.value === UInt(inputLimit)
  totalCounter.io.enable := pacman.io.digitOut.valid && !doneWithAllInput
  totalCounter.io.rst := Bool(false)

  io.leds.slice(0, 6).zipWithIndex.foreach
  {
    case (l, i) => {
      l := ~totalCounter.io.value(totalCounterWidth - 1 - i)
    }
  }
  io.leds(6) := currentResultCorrect
  io.leds(7) := !incorrectSwitch.io.state
}

