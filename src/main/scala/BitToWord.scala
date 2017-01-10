package Pacman

import scala.language.reflectiveCalls
import Chisel._

class BitToWord(K: Int) extends Module {
  val io = new Bundle {
    val enable = Bool().asInput
    val bit = Bits(width = 1).asInput
    val word = Bits(width = K).asOutput
  }

  val regs = Array.fill(K)(Reg(Bits(width=1)))
  when(io.enable){
    regs.zip(regs.drop(1))
      .foreach({
        case (a, b) => {
          b := a
        }
      })

    regs(0) := io.bit
  }
  io.word := regs.reduceLeft((a, b) => Cat(a, b))

}
