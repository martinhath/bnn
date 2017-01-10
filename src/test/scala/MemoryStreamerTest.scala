package Pacman

import Chisel._

class MemoryStreamerTests(ms: MemoryStreamer) extends Tester(ms) {

  poke(ms.io.restart, 1)
  step(1)
  peek(ms.io)

  expect(ms.io.bias, Integer.parseInt("01", 2))
  expect(ms.io.weights(0), Integer.parseInt("001"))
  expect(ms.io.weights(1), Integer.parseInt("010"))

  poke(ms.io.restart, 0)
  step(1)
  peek(ms.io)

  expect(ms.io.bias, Integer.parseInt("10", 2))
  expect(ms.io.weights(0), Integer.parseInt("100"))
  expect(ms.io.weights(1), Integer.parseInt("100"))

  poke(ms.io.restart, 0)
  step(1)
  peek(ms.io)

  expect(ms.io.bias, Integer.parseInt("00", 2))
  expect(ms.io.weights(0), Integer.parseInt("100"))
  expect(ms.io.weights(1), Integer.parseInt("001"))
  expect(ms.io.weights(2), Integer.parseInt("110"))
  expect(ms.io.weights(3), Integer.parseInt("111"))

  poke(ms.io.restart, 0)
  step(1)
  peek(ms.io)

  expect(ms.io.bias, Integer.parseInt("00", 2))
  expect(ms.io.weights(0), Integer.parseInt("000"))
  expect(ms.io.weights(1), Integer.parseInt("111"))
  expect(ms.io.weights(2), Integer.parseInt("001"))
  expect(ms.io.weights(3), Integer.parseInt("000"))

  // Restart after complete pass

  poke(ms.io.restart, 1)
  step(1)
  peek(ms.io)

  expect(ms.io.bias, Integer.parseInt("01", 2))
  expect(ms.io.weights(0), Integer.parseInt("001"))
  expect(ms.io.weights(1), Integer.parseInt("010"))
  expect(ms.io.weights(2), Integer.parseInt("111"))
  expect(ms.io.weights(3), Integer.parseInt("000"))

  poke(ms.io.restart, 0)
  step(1)
  peek(ms.io)

  expect(ms.io.bias, Integer.parseInt("10", 2))
  expect(ms.io.weights(0), Integer.parseInt("100"))
  expect(ms.io.weights(1), Integer.parseInt("100"))
  expect(ms.io.weights(2), Integer.parseInt("010"))
  expect(ms.io.weights(3), Integer.parseInt("001"))

  // Restart in the middle of a pass

  poke(ms.io.restart, 1)
  step(1)
  peek(ms.io)

  expect(ms.io.bias, Integer.parseInt("01", 2))
  expect(ms.io.weights(0), Integer.parseInt("001"))
  expect(ms.io.weights(1), Integer.parseInt("010"))
  expect(ms.io.weights(2), Integer.parseInt("110"))
  expect(ms.io.weights(3), Integer.parseInt("111"))

  poke(ms.io.restart, 0)
  step(1)
  peek(ms.io)

  expect(ms.io.bias, Integer.parseInt("10", 2))
  expect(ms.io.weights(0), Integer.parseInt("100"))
  expect(ms.io.weights(1), Integer.parseInt("100"))
  expect(ms.io.weights(2), Integer.parseInt("001"))
  expect(ms.io.weights(3), Integer.parseInt("000"))

  poke(ms.io.restart, 0)
  step(1)
  peek(ms.io)

  expect(ms.io.bias, Integer.parseInt("00", 2))
  expect(ms.io.weights(0), Integer.parseInt("100"))
  expect(ms.io.weights(1), Integer.parseInt("001"))
  expect(ms.io.weights(2), Integer.parseInt("110"))
  expect(ms.io.weights(3), Integer.parseInt("111"))

}

object MemoryStreamerTest {
  def main(args: Array[String]) {
    val margs = Array("--backend", "c", "--genHarness", "--compile", "--test")
    val parameters = new LayerParameters(
      K = 3,
      BiasWidth = 2,
      NumberOfPUs = 4,
      NumberOfMS = 2
    )
    val weights = Array(
      Array[String]("b010001", "b100100", "b001100", "b111000"),
      Array[String]("b111110", "b000001", "b000111", "b001010")
    )
    val bias = Array[String]("b01", "b10", "b00")
    chiselMainTest(margs, () => Module(new MemoryStreamer(parameters, weights, bias))) {
      c => new MemoryStreamerTests(c)
    }
  }
}
