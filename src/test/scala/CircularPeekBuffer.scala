package Pacman

import util.Random

import Chisel._

class CircularPeekBufferTests(c: CircularPeekBuffer) extends Tester(c) {
  val bits = (b: String) => {Integer.parseInt(b, 2)}
  def cycle(
    wordIn: String,
    validIn: Int,
    pipeReady: Int,
    startOut: Int,
    wordOut: String,
    readyOut: Int
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
    if(wordOut.length > 0) { expect(c.io.wordOut, bits(wordOut)) }
    expect(c.io.wordIn.ready, readyOut)
  }

  poke(c.io.wordIn.valid, false)
  step(100)

  //    wordIn  validIn  pipeReady         startOut  wordOut readyOut
  cycle(    "",       0,         1,  /**/         0,      "",       1)
  cycle(  "10",       1,         1,  /**/         0,      "",       1)
  cycle(  "01",       1,         1,  /**/         0,      "",       1)
  cycle(  "11",       1,         1,  /**/         0,      "",       1)
  cycle(    "",       0,         1,  /**/         1,    "10",       1)
  cycle(  "00",       1,         0,  /**/         0,    "01",       1)
  cycle(    "",       0,         0,  /**/         0,    "11",       1)
  cycle(  "11",       1,         0,  /**/         0,    "10",       1)
  cycle(  "10",       1,         0,  /**/         0,    "01",       1)
  cycle(  "01",       1,         0,  /**/         0,    "11",       1)
  cycle(  "01",       1,         0,  /**/         0,    "10",       1)
  cycle(  "10",       1,         0,  /**/         0,    "01",       0)
  cycle(    "",       0,         0,  /**/         0,    "11",       0)
  cycle(    "",       0,         0,  /**/         0,    "10",       0)
  cycle(    "",       0,         1,  /**/         1,    "00",       1)
  cycle(  "10",       1,         0,  /**/         0,    "11",       1)
  cycle(  "11",       1,         0,  /**/         0,    "10",       1)
  cycle(  "11",       1,         1,  /**/         1,    "01",       1)
  cycle(    "",       0,         0,  /**/         0,    "01",       1)
  cycle(    "",       0,         0,  /**/         0,    "10",       1)
  cycle(    "",       0,         1,  /**/         1,    "10",       1)
  cycle(    "",       0,         1,  /**/         0,    "11",       1)
  cycle(    "",       0,         1,  /**/         0,    "11",       1)
  cycle(    "",       0,         1,  /**/         0,    "10",       1)
  cycle(    "",       0,         1,  /**/         0,    "11",       1)
  cycle(    "",       0,         1,  /**/         0,    "11",       1)

}

object CircularPeekBufferTest {
  def main(args: Array[String]) {
    val margs = Array("--backend", "c", "--genHarness", "--compile", "--test")
    chiselMainTest(margs, () => Module(
                     new CircularPeekBuffer(
                       numberOfBlocks=3,
                       wordPerBlock=3,
                       wordWidth=2)
                   )) {
      c => new CircularPeekBufferTests(c)
    }
  }
}
