package Pacman

import Chisel._

class Net(
  layers: List[LayerData]) extends Module {

  val io = new Bundle {
    val ready = Bool().asOutput
    val start = Bool().asInput
    val xsIn = Vec.fill(layers(0).parameters.NumberOfCores)(Bits(width = layers(0).parameters.K)).asInput

    val xsOut = Vec.fill(layers.last.parameters.NumberOfCores)(Bits(width = 1)).asOutput
    val xsOutValid = Bool().asOutput
    val done = Bool().asOutput
    val pipeReady = Bool().asInput
  }

  val warps = layers.map(layer => {
    Module(new Warp(layer))
  })

  val gearBoxes = layers.zip(layers.drop(1))
    .map {
      case (a, b) => {
        val gearBoxParameters = new GearBoxParameters(a.parameters, b.parameters)
        Module(new GearBox(gearBoxParameters))
      }
    }

  // Hook up each warp to the gear box behind it
  warps.zip(gearBoxes).foreach{
    case (warp, gearBox) => {
      gearBox.io.xsIn := warp.io.xOut
      gearBox.io.validIn := warp.io.xOutValid
      gearBox.io.prevDone := warp.io.done
      gearBox.io.prevStart := warp.io.startOut

      warp.io.pipeReady := gearBox.io.ready
    }
  }

  // Hook up each warp to the gear box in front of it
  gearBoxes.zip(warps.drop(1)).foreach{
    case (gearBox, warp) => {
      warp.io.start := gearBox.io.startNext
      warp.io.xIn := gearBox.io.xsOut

      gearBox.io.nextReady := warp.io.ready
    }
  }

  // Hook up first warp to net io
  io.ready := warps(0).io.ready
  warps(0).io.start := io.start
  warps(0).io.xIn := io.xsIn

  // Hook up last warp to net io
  io.xsOut := warps.last.io.xOut
  io.xsOutValid := warps.last.io.xOutValid
  io.done := warps.last.io.done
  warps.last.io.pipeReady := io.pipeReady
}
