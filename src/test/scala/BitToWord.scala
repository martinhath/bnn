package Pacman

import Chisel._

class BitToWordTests(btw : BitToWord) extends Tester(btw) {
  def push(bit : Int) {
    poke(btw.io.enable, true)
    poke(btw.io.bit, bit)
    step(1)
  }

  def disabledPush(bit : Int) {
    poke(btw.io.enable, false)
    poke(btw.io.bit, bit)
    step(1)
  }

  disabledPush(0)
  disabledPush(1)
  disabledPush(0)
  disabledPush(0)
  disabledPush(1)

  push(1)
  push(1)
  push(0)
  push(1)
  expect(btw.io.word, 11)

  push(1)
  push(0)
  disabledPush(1)
  disabledPush(1)
  push(0)
  push(0)
  expect(btw.io.word, 1)

  push(1)
  push(0)
  push(1)
  push(0)
  push(1)
  push(0)
  push(1)
  push(0)
  push(1)
  expect(btw.io.word, 10)
}

object BitToWordTest {
  def main(args: Array[String]) {
    val margs = Array("--backend", "c", "--genHarness", "--compile", "--test")
    chiselMainTest(margs, () => Module(new BitToWord(4))) {
      c => new BitToWordTests(c)
    }
  }
}
