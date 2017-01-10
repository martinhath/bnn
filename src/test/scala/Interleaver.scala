
package Pacman

import util.Random
import Chisel._

class InterleaverTests1(c: Interleaver, p: LayerParameters) extends Tester(c) {
  val bits = (b: String) => {Integer.parseInt(b, 2)}
  def i(is: String*): Array[String] = { is.toArray }
  def cycle(
    wordIn: String,
    pipeReady: Int,
    startOut: Int,
    wordsOut: Array[String],
    ready: Int
  ) {

    if(wordIn.length > 0) {
      poke(c.io.wordIn.bits, bits(wordIn))
      poke(c.io.wordIn.valid, true)
    }
    else {
      poke(c.io.wordIn.bits, Random.nextInt(12345))
      poke(c.io.wordIn.valid, false)
    }
    poke(c.io.pipeReady, pipeReady)
    step(1)
    expect(c.io.startOut, startOut)
    if(wordsOut.length > 0) {
      for(i <- 0 until p.NumberOfCores) {
        expect(c.io.interleavedOut(i), bits(wordsOut(i)))
      }
    }
    expect(c.io.wordIn.ready, ready)
  }

  poke(c.io.wordIn.valid, false)
  step(100)

  //    wordIn  pipeReady -> startOut           wordsOut  ready
  cycle(  "",           1,          0,               i(),     1) // 101
  cycle("11",           1,          0,               i(),     1)
  cycle("10",           1,          0,               i(),     1)
  cycle("01",           1,          0,               i(),     1)

  cycle("10",           1,          0,               i(),     1) // 105
  cycle(  "",           1,          0,               i(),     1)
  cycle("11",           1,          0,               i(),     1)
  cycle("01",           1,          0,               i(),     1)

  cycle("00",           1,          0,               i(),     1) // 109
  cycle("10",           1,          0,               i(),     1)
  cycle("01",           1,          0,               i(),     1)

  cycle("10",           1,          1, i("11","10","00"),     1) // 112
  cycle("11",           1,          0, i("10","11","10"),     1)
  cycle("11",           1,          0, i("01","01","01"),     1)

  cycle("00",           1,          0, i("11","10","00"),     1) // 115
  cycle("01",           1,          0, i("10","11","10"),     1)
  cycle("00",           1,          0, i("01","01","01"),     1)

  cycle("01",           1,          0, i("11","10","00"),     1) // 118
  cycle("01",           1,          0, i("10","11","10"),     1)
  cycle("10",           1,          0, i("01","01","01"),     1)


  cycle("11",           0,          0, i("11","10","00"),     1) // 121
  cycle("01",           0,          0, i("10","11","10"),     1)
  cycle("10",           0,          0, i("01","01","01"),     1)

  cycle("01",           0,          0, i("11","10","00"),     1) // 124
  cycle("11",           0,          0, i("10","11","10"),     1)
  cycle("10",           0,          0, i("01","01","01"),     1)

  cycle("00",           0,          0, i("11","10","00"),     1) // 127
  cycle("11",           0,          0, i("10","11","10"),     1)
  cycle("01",           0,          0, i("01","01","01"),     0)

  cycle("11",           0,          0, i("11","10","00"),     0) // 130
  cycle("11",           0,          0, i("10","11","10"),     0)
  cycle("11",           0,          0, i("01","01","01"),     0)
  cycle("11",           0,          0, i("11","10","00"),     0)
  cycle("11",           0,          0, i("10","11","10"),     0)
  cycle("11",           0,          0, i("01","01","01"),     0)

  cycle(  "",           1,          1, i("10","00","01"),     1) // 136
  cycle("10",           0,          0, i("11","01","01"),     1)
  cycle(  "",           0,          0, i("11","00","10"),     1)

  cycle(  "",           1,          1, i("11","01","00"),     1) // 139
  cycle("11",           1,          0, i("01","11","11"),     1)
  cycle(  "",           1,          0, i("10","10","01"),     1)

  cycle(  "",           1,          0, i("11","01","00"),     1) // 142
  cycle(  "",           1,          0, i("01","11","11"),     1)
  cycle("01",           1,          0, i("10","10","01"),     1)

  cycle("00",           1,          0, i("11","01","00"),     1) // 145
  cycle("01",           1,          0, i("01","11","11"),     1)
  cycle("11",           1,          0, i("10","10","01"),     1)

  cycle("10",           1,          0, i("11","01","00"),     1) // 148
  cycle("10",           1,          0, i("01","11","11"),     1)
  cycle("11",           1,          0, i("10","10","01"),     1)

  cycle("00",           0,          0, i("11","01","00"),     1) // 151

  cycle("00",           1,          1, i("10","00","10"),     1) // 152
  cycle("00",           1,          0, i("11","01","10"),     1)
  cycle("00",           1,          0, i("01","11","11"),     1)
}

class InterleaverTests2(c: Interleaver, p: LayerParameters) extends Tester(c) {
  val bits = (b: String) => {Integer.parseInt(b, 2)}
  def i(is: String*): Array[String] = { is.toArray }
  def cycle(
    wordIn: String,
    pipeReady: Int,
    startOut: Int,
    wordsOut: Array[String],
    ready: Int
  ) {

    if(wordIn.length > 0) {
      poke(c.io.wordIn.bits, bits(wordIn))
      poke(c.io.wordIn.valid, true)
    }
    else {
      poke(c.io.wordIn.bits, Random.nextInt(12345))
      poke(c.io.wordIn.valid, false)
    }
    poke(c.io.pipeReady, pipeReady)
    step(1)
    expect(c.io.startOut, startOut)
    if(wordsOut.length > 0) {
      for(i <- 0 until p.NumberOfCores) {
        expect(c.io.interleavedOut(i), bits(wordsOut(i)))
      }
    }
    expect(c.io.wordIn.ready, ready)
  }

  poke(c.io.wordIn.valid, false)
  step(100)

  //    wordIn  pipeReady -> startOut           wordsOut  ready
  cycle(  "",           1,          0,               i(),     1) // 101
  cycle("11",           1,          0,               i(),     1)
  cycle("10",           1,          0,               i(),     1)
  cycle("01",           1,          0,               i(),     1)

  cycle("10",           1,          1,           i("11"),     1)
  cycle("11",           0,          0,           i("10"),     1)
  cycle("11",           0,          0,           i("01"),     1)

  cycle("01",           1,          1,           i("10"),     1)
  cycle("00",           0,          0,           i("11"),     1)
  cycle("00",           0,          0,           i("11"),     1)

  cycle("00",           0,          0,           i("10"),     1)
  cycle(  "",           0,          0,           i("11"),     1)
  cycle("11",           0,          0,           i("11"),     1)
  cycle("10",           0,          0,           i("10"),     0)

  cycle("11",           0,          0,           i("11"),     0)
  cycle("11",           0,          0,           i("11"),     0)

  cycle("11",           1,          1,           i("01"),     1)
  cycle(  "",           0,          0,           i("00"),     1)
  cycle("10",           0,          0,           i("00"),     1)

  cycle(  "",           1,          1,           i("00"),     1)
  cycle(  "",           0,          0,           i("11"),     1)
  cycle(  "",           0,          0,           i("10"),     1)

  cycle(  "",           1,          0,           i("00"),     1)
  cycle(  "",           1,          0,           i("11"),     1)
  cycle(  "",           1,          0,           i("10"),     1)
}

object InterleaverTest {
  def main(args: Array[String]) {
    val margs = Array("--backend", "c", "--genHarness", "--compile", "--test")
    val p1 = new LayerParameters(
      K = 2,
      BiasWidth = 8,
      AccumulatorWidth = 10,
      NumberOfPUs = 16,
      AddressWidth = 0,
      NumberOfMS = 8,
      MatrixWidth = 6,
      MatrixHeight = 256,
      NumberOfCores = 3
    )
    val p2 = new LayerParameters(
      K = 2,
      BiasWidth = 8,
      AccumulatorWidth = 10,
      NumberOfPUs = 16,
      AddressWidth = 0,
      NumberOfMS = 8,
      MatrixWidth = 6,
      MatrixHeight = 256,
      NumberOfCores = 1
    )
    chiselMainTest(margs, () => Module(new Interleaver(p1))) {
      c => new InterleaverTests1(c, p1)
    }
    chiselMainTest(margs, () => Module(new Interleaver(p2))) {
      c => new InterleaverTests2(c, p2)
    }
  }
}
