package Pacman

import Chisel._
import util.Random

class WidthConverterTestPassthrough(c: WidthConverter) extends Tester(c) {
  poke(c.io.wordIn.valid, true)
  expect(c.io.wordOut.valid, true)
  poke(c.io.wordIn.valid, false)
  expect(c.io.wordOut.valid, false)

  poke(c.io.wordIn.bits, 3)
  expect(c.io.wordOut.bits, 3)
  poke(c.io.wordIn.bits, 0)
  expect(c.io.wordOut.bits, 0)

  poke(c.io.wordOut.ready, true)
  expect(c.io.wordIn.ready, true)
  poke(c.io.wordOut.ready, false)
  expect(c.io.wordIn.ready, false)
}

class WidthConverterTest2To6(c: WidthConverter) extends Tester(c) {
  def peeks() {

  }
  def cycle(
    wordOutReady: Int,
    wordInBits: Int,
    wordInReady: Int,
    wordOutBits: Int
  ) {
    poke(c.io.wordOut.ready, wordOutReady)
    if(wordInBits != -1) {
      poke(c.io.wordIn.valid, true)
      poke(c.io.wordIn.bits, wordInBits)
    }
    else {
      poke(c.io.wordIn.valid, false)
      poke(c.io.wordIn.bits, Random.nextInt(123456))
    }
    peeks()
    step(1)
    expect(c.io.wordIn.ready, wordInReady)
    if(wordOutBits != -1) {
      expect(c.io.wordOut.bits, wordOutBits)
      expect(c.io.wordOut.valid, true)
    }
    else {
      expect(c.io.wordOut.valid, false)
    }
  }

  step(100)

  //    wordOutReady  wordInBits -> wordInReady  wordOutBits
  cycle(           0,         -1,             1,          -1) // 101
  cycle(           0,         -1,             1,          -1) // 102
  cycle(           0,          3,             1,          -1)
  cycle(           0,          2,             1,          -1)
  cycle(           0,          1,             0,          27) // 105
  cycle(           0,          2,             0,          27)
  cycle(           0,          2,             0,          27)
  cycle(           0,          2,             0,          27)
  cycle(           1,          2,             1,          -1) // 109
  cycle(           1,          0,             1,          -1)
  cycle(           1,         -1,             1,          -1)
  cycle(           1,          3,             1,          50)
  cycle(           1,          1,             1,          -1)
  cycle(           1,          3,             1,          -1)
  cycle(           1,          2,             1,          45) // 115
  cycle(           1,         -1,             1,          -1)
  cycle(           1,         -1,             1,          -1)
  cycle(           1,         -1,             1,          -1)
  cycle(           1,         -1,             1,          -1)
}

class WidthConverterTest6To2(c: WidthConverter) extends Tester(c) {
  def peeks() {

  }
  def cycle(
    wordOutReady: Int,
    wordInBits: Int,
    wordInReady: Int,
    wordOutBits: Int
  ) {
    poke(c.io.wordOut.ready, wordOutReady)
    if(wordInBits != -1) {
      poke(c.io.wordIn.valid, true)
      poke(c.io.wordIn.bits, wordInBits)
    }
    else {
      poke(c.io.wordIn.valid, false)
      poke(c.io.wordIn.bits, Random.nextInt(123456))
    }
    peeks()
    step(1)
    expect(c.io.wordIn.ready, wordInReady)
    if(wordOutBits != -1) expect(c.io.wordOut.bits, wordOutBits)
  }

  step(100)

  //    wordOutReady  wordInBits -> wordInReady  wordOutBits
  cycle(           1,         -1,             1,          -1) // 101
  cycle(           1,         57,             0,           1)
  cycle(           1,         34,             0,           2)
  cycle(           1,         34,             1,           3)
  cycle(           1,         34,             0,           2)
  cycle(           0,         -1,             0,           2)
  cycle(           1,         -1,             0,           0)
  cycle(           0,         -1,             0,           0)
  cycle(           1,         -1,             1,           2)
  cycle(           1,         -1,             1,          -1) // 110
  cycle(           1,         -1,             1,          -1)
  cycle(           0,         -1,             1,          -1)
  cycle(           0,         54,             0,           2)
  cycle(           0,          0,             0,           2)
  cycle(           1,          0,             0,           1)
  cycle(           1,          0,             1,           3)
}

object WidthConverterTest {
  def main(args: Array[String]) {
    val margs = Array("--backend", "c", "--genHarness", "--compile", "--test", "--debug")
    val tests = Array(
      (() => Module(new WidthConverter(3, 3)), c => new WidthConverterTestPassthrough(c)),
      (() => Module(new WidthConverter(2, 6)), c => new WidthConverterTest2To6(c)),
      (() => Module(new WidthConverter(6, 2)), c => new WidthConverterTest6To2(c))
    )


    tests.foreach{
      case(m, t) => {
        chiselMainTest(margs, m) {t}
      }
    }
  }
}
