package Pacman

import Chisel._

class GearBoxTests(c: GearBox, p: GearBoxParameters) extends Tester(c) {
  def peeks() {
    peek(c.io)
    peek(c.fillingBlock.io)
    peek(c.bitCounter.io)
    peek(c.blocksReady.io)
    peek(c.queues(0).io)
  }

  def cycle(xsIn: Int,
    validIn: Int,
    prevDone: Int,
    prevStart: Int,
    nextReady: Int,
    ready: Int,
    startNext: Int,
    xsOut: Int) {
      if (xsIn >= 0)      poke(c.io.xsIn(0), xsIn)
      if (validIn >= 0)   poke(c.io.validIn, validIn)
      if (prevDone >= 0)  poke(c.io.prevDone, prevDone)
      if (prevStart >= 0) poke(c.io.prevStart, prevStart)
      if (nextReady >= 0) poke(c.io.nextReady, nextReady)
      step(1)
    poke(c.io.prevStart, false)
      if (ready >= 0)     expect(c.io.ready, ready)
      if (startNext >= 0) expect(c.io.startNext, startNext)
      if (xsOut >= 0)     expect(c.io.xsOut(0), xsOut)
  }

  poke(c.io.prevStart, false)
  poke(c.io.validIn, false)
  poke(c.io.prevDone, false)
  poke(c.io.prevStart, false)
  poke(c.io.nextReady, false)
  poke(c.io.xsIn(0), 0)
  step(80)
  expect(c.io.ready, true)
  expect(c.io.startNext, false)
  poke(c.io.prevStart, true)
  step(1)
  poke(c.io.prevStart, false)
  step(19) // 100

  //    xsIn validIn prevDone prevStart nextReady     ready startNext xsOut
  cycle(  1,      1,       0,        0,        1,        1,        0,    -1) // 101
  cycle(  0,      1,       0,        0,        1,        1,        0,    -1)
  cycle(  0,      0,       0,        0,        1,        1,        0,    -1)
  cycle(  0,      1,       0,        0,        1,        1,        0,    -1)
  cycle(  1,      1,       0,        1,        1,        0,        0,    -1)
  cycle(  1,      1,       0,        0,        1,        0,        0,    -1)
  cycle(  1,      1,       1,        0,        1,        0,        0,    -1)
  // Numbers must propagate through the buffer and the queue before output
  cycle( -1,      0,       0,        0,        1,        0,        0,    -1)
  // We get output
  cycle( -1,      0,       0,        0,        1,        1,        1,     1)
  cycle(  1,      1,       0,        0,        0,        1,        0,     2) // 110
  cycle(  1,      1,       0,        0,        1,        1,        0,     3)
  cycle(  0,      1,       0,        0,        1,        1,        0,     1)
  cycle(  1,      1,       0,        0,        1,        1,        0,     2)
  cycle(  1,      1,       0,        0,        1,        1,        0,     3)
  cycle(  0,      1,       1,        1,        0,        0,        0,     1)
  // Fill up queue
  cycle(  1,      1,       0,        0,        0,        0,        0,     2) // 116
  cycle(  0,      1,       0,        0,        0,        0,        0,     3) // 117
  cycle(  1,      1,       0,        0,        0,        0,        0,     1) // 118
  cycle(  0,      1,       0,        0,        0,        0,        0,     2) // 119
  cycle(  0,      1,       0,        0,        0,        0,        0,     3) // 120
  cycle(  1,      1,       1,        0,        0,        0,        0,     1) // 121
  cycle( -1,      0,       0,        0,        0,        0,        0,     2) // 122
  // Read the two blocks as fast as possible
  cycle( -1,     -1,      -1,       -1,        1,       -1,        1,     3) // 123
  cycle( -1,     -1,      -1,       -1,        0,       -1,        0,     2) // 124
  cycle( -1,     -1,      -1,       -1,        0,       -1,        0,     1) // 125
  cycle( -1,     -1,      -1,       -1,        1,       -1,        1,     1) // 126
  cycle( -1,     -1,      -1,       -1,        0,       -1,        0,     1) // 127
  cycle( -1,     -1,      -1,       -1,        0,       -1,        0,     2) // 128
}

class GearBoxTests5x2(c: GearBox, p: GearBoxParameters) extends Tester(c) {
  def peeks() {
    peek(c.io)
    peek(c.fillingBlock.io)
    peek(c.bitCounter.io)
    peek(c.blocksReady.io)
    peek(c.queues(0).io)
    peek(c.queues(1).io)
    peek(c.queues(2).io)
    peek(c.queues(3).io)
    peek(c.queues(4).io)
    peek(c.outputSelectCounters(0).io)
    peek(c.outputSelectCounters(1).io)
    peek(c.outputSelectCounters(2).io)
    peek(c.outputSelectCounters(3).io)
    peek(c.outputSelectCounters(4).io)
    peek(c.queueOutputSelectCounters(0).io)
    peek(c.queueOutputSelectCounters(1).io)
  }

  def cycle(xsIn: Array[Int],
    validIn: Int,
    prevDone: Int,
    prevStart: Int,
    nextReady: Int,
    ready: Int,
    startNext: Int,
    xsOut: Array[Int]) {
      if (xsIn.length > 0)
        for (i <- 0 until p.Previous.NumberOfCores)
          poke(c.io.xsIn(i), xsIn(i))
      if (validIn >= 0)   poke(c.io.validIn, validIn)
      if (prevDone >= 0)  poke(c.io.prevDone, prevDone)
      if (prevStart >= 0) poke(c.io.prevStart, prevStart)
      if (nextReady >= 0) poke(c.io.nextReady, nextReady)
    peeks()
      step(1)
    poke(c.io.prevStart, false)
      if (ready >= 0)     expect(c.io.ready, ready)
      if (startNext >= 0) expect(c.io.startNext, startNext)
      if (xsOut.length > 0)
        for (i <- 0 until p.Next.NumberOfCores)
          expect(c.io.xsOut(i), xsOut(i))
  }

  poke(c.io.prevStart, false)
  poke(c.io.validIn, false)
  poke(c.io.prevDone, false)
  poke(c.io.prevStart, false)
  poke(c.io.nextReady, false)
  // poke(c.io.xsIn(0), 0)
  step(80)
  expect(c.io.ready, true)
  expect(c.io.startNext, false)
  poke(c.io.prevStart, true)
  step(1)
  poke(c.io.prevStart, false)
  step(19) // 100

  def i(is: Int*): Array[Int] = { is.toArray }

  //       xsIn       validIn prevDone prevStart nextReady /**/ ready startNext xsOut
  cycle(i(1,0,0,1,0),      1,       0,        0,        1, /**/    1,        0, i(    )) // 101
  cycle(i(1,0,1,0,1),      0,       0,        0,        1, /**/    1,        0, i(    )) // 102
  cycle(i(0,0,1,1,0),      1,       0,        0,        1, /**/    1,        0, i(    )) // 103
  cycle(i(1,1,1,1,1),      1,       0,        0,        1, /**/    1,        0, i(    )) // 104
  cycle(i(1,1,1,0,0),      1,       0,        0,        1, /**/    1,        0, i(    )) // 105
  cycle(i(0,0,0,0,1),      1,       0,        0,        1, /**/    1,        0, i(    )) // 106
  cycle(i(1,0,0,0,1),      1,       1,        0,        1, /**/    1,        0, i(    )) // 107

  cycle(i(         ),      0,       0,        0,        1, /**/    1,        0, i(    )) // 108
  cycle(i(         ),      0,       0,        0,        1, /**/    1,        1, i(1, 0)) // 109
  cycle(i(         ),      0,       0,        0,        0, /**/    1,        0, i(3, 3)) // 110
  cycle(i(         ),      0,       0,        0,        0, /**/    1,        0, i(2, 0)) // 111
  cycle(i(         ),      0,       0,        0,        0, /**/    1,        0, i(1, 0)) // 112

  cycle(i(         ),      0,       0,        0,        1, /**/    1,        1, i(2, 3)) // 113
  cycle(i(         ),      0,       0,        1,        0, /**/    1,        0, i(3, 1)) // 114
  cycle(i(         ),      0,       0,        0,        1, /**/    1,        0, i(0, 0)) // 115
  cycle(i(         ),      0,       0,        0,        1, /**/    1,        0, i(2, 3)) // 116
  cycle(i(         ),      0,       0,        0,        1, /**/    1,        0, i(3, 1)) // 117
  cycle(i(         ),      0,       0,        0,        1, /**/    1,        0, i(0, 0)) // 118

  cycle(i(1,1,1,0,0),      1,       0,        0,        1, /**/    1,        0, i(2, 3)) // 119
  cycle(i(1,0,0,1,1),      1,       0,        0,        1, /**/    1,        0, i(3, 1)) // 120
  cycle(i(0,0,1,1,1),      1,       0,        0,        1, /**/    1,        0, i(0, 0)) // 121
  cycle(i(1,1,0,1,0),      1,       0,        1,        1, /**/    0,        0, i(2, 3)) // 122
  cycle(i(1,1,0,1,0),      1,       0,        0,        1, /**/    0,        0, i(3, 1)) // 123
  cycle(i(0,1,0,1,1),      1,       1,        0,        1, /**/    0,        0, i(0, 0)) // 124

  cycle(i(0,0,0,1,1),      1,       0,        0,        0, /**/    0,        0, i(2, 3)) // 125
  cycle(i(1,1,1,1,0),      1,       0,        0,        0, /**/    0,        0, i(3, 1)) // 126
  cycle(i(0,1,0,0,0),      1,       0,        0,        0, /**/    0,        0, i(0, 0)) // 127
  cycle(i(1,1,0,1,1),      1,       0,        0,        0, /**/    0,        0, i(2, 3)) // 128
  cycle(i(0,1,0,0,0),      1,       0,        0,        0, /**/    0,        0, i(3, 1)) // 129
  cycle(i(1,0,1,0,1),      1,       1,        0,        0, /**/    0,        0, i(0, 0)) // 130

  cycle(i(         ),      0,       0,        0,        0, /**/    0,        0, i(2, 3)) // 131
  cycle(i(         ),      0,       0,        0,        0, /**/    0,        0, i(3, 1)) // 132

  cycle(i(         ),      0,       0,        0,        1, /**/    0,        1, i(0, 3)) // 133
  cycle(i(         ),      0,       0,        0,        0, /**/    0,        0, i(1, 2)) // 134
  cycle(i(         ),      0,       0,        0,        0, /**/    0,        0, i(3, 1)) // 135

  cycle(i(         ),      0,       0,        0,        1, /**/    1,        1, i(1, 1)) // 136
  cycle(i(         ),      0,       0,        0,        0, /**/    1,        0, i(2, 1)) // 137
  cycle(i(         ),      0,       0,        0,        0, /**/    1,        0, i(3, 0)) // 138
}

class GearBoxTests2x3(c: GearBox, p: GearBoxParameters) extends Tester(c) {
  def peeks() {
    peek(c.io)
    peek(c.fillingBlock.io)
    peek(c.bitCounter.io)
    peek(c.blocksReady.io)
    peek(c.queues(0).io)
    peek(c.queues(1).io)
    peek(c.queues(2).io)
    peek(c.inputSelectCounters(0).io)
    peek(c.inputSelectCounters(1).io)
    peek(c.inputSelectCounters(2).io)
    peek(c.outputSelectCounters(0).io)
    peek(c.outputSelectCounters(1).io)
    peek(c.outputSelectCounters(2).io)
    peek(c.queueOutputSelectCounters(0).io)
    peek(c.queueOutputSelectCounters(1).io)
    peek(c.queueOutputSelectCounters(2).io)
  }

  def cycle(xsIn: Array[Int],
    validIn: Int,
    prevDone: Int,
    prevStart: Int,
    nextReady: Int,
    ready: Int,
    startNext: Int,
    xsOut: Array[Int]) {
      if (xsIn.length > 0)
        for (i <- 0 until p.Previous.NumberOfCores)
          poke(c.io.xsIn(i), xsIn(i))
      if (validIn >= 0)   poke(c.io.validIn, validIn)
      if (prevDone >= 0)  poke(c.io.prevDone, prevDone)
      if (prevStart >= 0) poke(c.io.prevStart, prevStart)
      if (nextReady >= 0) poke(c.io.nextReady, nextReady)
    peek(c.blocksReady.io)
      step(1)
      poke(c.io.prevStart, false)
      poke(c.io.prevDone, false)
      if (ready >= 0)     expect(c.io.ready, ready)
      if (startNext >= 0) expect(c.io.startNext, startNext)
      if (xsOut.length > 0)
        for (i <- 0 until p.Next.NumberOfCores)
          expect(c.io.xsOut(i), xsOut(i))
  }

  poke(c.io.prevStart, false)
  poke(c.io.validIn, false)
  poke(c.io.prevDone, false)
  poke(c.io.prevStart, false)
  poke(c.io.nextReady, false)
  peek(c.blocksReady.io)
  step(80)
  peek(c.blocksReady.io)
  expect(c.io.ready, true)
  expect(c.io.startNext, false)
  poke(c.io.prevStart, true)
  step(1)
  poke(c.io.prevStart, false)
  step(19) // 100

  def i(is: Int*): Array[Int] = { is.toArray }

  //       xsIn  validIn prevDone prevStart nextReady /**/ ready startNext xsOut
  cycle(i(1, 1),      1,       0,        0,        1, /**/    1,        0, i(       )) // 101
  cycle(i(9, 9),      0,       0,        0,        1, /**/    1,        0, i(       )) // 102
  cycle(i(1, 1),      1,       0,        0,        1, /**/    1,        0, i(       )) // 103

  cycle(i(0, 0),      1,       0,        0,        1, /**/    1,        0, i(       )) // 104
  cycle(i(1, 0),      1,       0,        1,        1, /**/    1,        0, i(       )) // 105

  cycle(i(1, 1),      1,       0,        0,        1, /**/    1,        0, i(       )) // 106
  cycle(i(0, 1),      1,       1,        0,        1, /**/    1,        0, i(       )) // 107

  cycle(i(0, 1),      1,       0,        0,        1, /**/    1,        0, i(       )) // 108
  cycle(i(0, 0),      1,       0,        0,        1, /**/    1,        0, i(       )) // 109

  cycle(i(0, 0),      1,       0,        0,        1, /**/    1,        0, i(       )) // 110
  cycle(i(0, 1),      1,       0,        0,        1, /**/    1,        0, i(       )) // 111

  cycle(i(1, 0),      1,       0,        0,        1, /**/    1,        0, i(       )) // 112
  cycle(i(0, 1),      1,       1,        0,        1, /**/    1,        0, i(       )) // 113

  cycle(i(    ),      1,       0,        0,        1, /**/    1,        0, i(       )) // 113

  cycle(i(    ),      1,       0,        0,        1, /**/    1,        1, i(3, 3, 0)) // 113
  // It worked
}

object GearBoxTest {
  def main(args: Array[String]) {
    val margs = Array("--backend", "c", "--genHarness", "--compile", "--test")
    // Random.setSeed(12)
    if (true) {
      val p = new GearBoxParameters(
        new LayerParameters(
          MatrixHeight=6,
          NumberOfCores=1
        ), new LayerParameters(
          K=2,
          NumberOfCores=1
        ))
      chiselMainTest(margs, () => Module(new GearBox(p))) {
        c => new GearBoxTests(c, p)
      }
    }
    if (true) {
      val p = new GearBoxParameters(
        new LayerParameters(
          MatrixHeight=6,
          NumberOfCores=5
        ), new LayerParameters(
          K=2,
          NumberOfCores=2
        ))
      chiselMainTest(margs, () => Module(new GearBox(p))) {
        c => new GearBoxTests5x2(c, p)
      }
    }
    if (true) {
      val p = new GearBoxParameters(
        new LayerParameters(
          MatrixHeight=6,
          NumberOfCores=2
        ), new LayerParameters(
          K=2,
          NumberOfCores=3
        ))
      chiselMainTest(margs, () => Module(new GearBox(p))) {
        c => new GearBoxTests2x3(c, p)
      }
    }
  }
}
