package Pacman

import Chisel._

class WidthConverter(widthIn: Int, widthOut: Int) extends Module {
  def gcd(a: Int, b: Int) : Int = {
    if(b == 0) a
    else gcd(b, a % b)
  }
  def lcm(a: Int, b: Int) : Int = {
    (a * b) / gcd(a, b)
  }
  val io = new Bundle {
    val wordIn = Decoupled(Bits(width=widthIn)).flip
    val wordOut = Decoupled(Bits(width=widthOut))
  }

  if(widthIn == widthOut) {
    io.wordIn <> io.wordOut
  }
  else if(widthOut % widthIn == 0) {
    val multiple = widthOut / widthIn

    val regs = Array.fill(multiple)(Reg(Bits(width=widthIn)))
    val smallWordsCounter = Module(new CounterWithNonBlockingReset(0, multiple + 1))

    val isFull = smallWordsCounter.io.value === UInt(multiple)
    val isReady = !isFull || io.wordOut.ready
    val signalWordIn = isReady && io.wordIn.valid
    val signalWordOut = isFull && io.wordOut.ready

    smallWordsCounter.io.enable := signalWordIn
    smallWordsCounter.io.rst := signalWordOut

    when(signalWordIn){
      regs.zip(regs.drop(1))
        .foreach
        {
          case (a, b) => {
            b := a
          }
        }
      regs(0) := io.wordIn.bits
    }.otherwise {
      regs.foreach(reg => reg := reg)
    }

    io.wordIn.ready := isReady
    io.wordOut.bits := regs.reduceLeft((a, b) => Cat(a, b))
    io.wordOut.valid := isFull
  }
  else if(widthIn % widthOut == 0) {
    val multiple = widthIn / widthOut

    val regs = Vec.fill(multiple)(Reg(Bits(width=widthOut)))
    val smallWordsCounter = Module(new Counter(0, multiple))

    val hasBigWord = Reg(init=Bool(false))
    val signalWordOut = hasBigWord && io.wordOut.ready
    val signalLastWordOut = (smallWordsCounter.io.value === UInt(multiple - 1)) && signalWordOut
    val isReady = !hasBigWord || signalLastWordOut
    val signalWordIn = isReady && io.wordIn.valid
    val signalWillBeEmpty = signalLastWordOut && !signalWordIn

    when(signalWordIn) {
      hasBigWord := Bool(true)
    }.elsewhen(signalWillBeEmpty) {
      hasBigWord := Bool(false)
    }.otherwise {
      hasBigWord := hasBigWord
    }

    smallWordsCounter.io.enable := signalWordOut
    smallWordsCounter.io.rst := signalLastWordOut

    when(signalWordIn) {
      regs.zipWithIndex.foreach
      {
        case (reg, i) => {
          reg := io.wordIn.bits((i + 1) * widthOut - 1, i * widthOut)
        }
      }
    }.otherwise {
      regs := regs
    }

    io.wordIn.ready := isReady
    io.wordOut.bits := regs(smallWordsCounter.io.value)
    io.wordOut.valid := hasBigWord
  }
  else {
    val widthMid = lcm(widthIn, widthOut)
    val first = Module(new WidthConverter(widthIn, widthMid))
    val second = Module(new WidthConverter(widthMid, widthOut))

    io.wordIn <> first.io.wordIn
    first.io.wordOut <> second.io.wordIn
    second.io.wordOut <> io.wordOut
  }
}
