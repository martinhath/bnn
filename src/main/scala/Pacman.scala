package Pacman

import Chisel._

object PacmanSetup {
  def getLayers : List[LayerData] = {
    val testData = Utils.readDumpFile()
    val parametersList = Array(
      new LayerParameters(
        K = 16,
        BiasWidth = 8,
        AccumulatorWidth = 10,
        NumberOfPUs = 32,
        AddressWidth = 0,
        NumberOfMS = 16,
        MatrixWidth = 784,
        MatrixHeight = 256,
        NumberOfCores = 3
      ),
      new LayerParameters(
        K = 16,
        BiasWidth = 8,
        AccumulatorWidth = 10,
        NumberOfPUs = 16,
        AddressWidth = 0,
        NumberOfMS = 8,
        MatrixWidth = 256,
        MatrixHeight = 256,
        NumberOfCores = 3
      ),
      new LayerParameters(
        K = 16,
        BiasWidth = 8,
        AccumulatorWidth = 10,
        NumberOfPUs = 16,
        AddressWidth = 0,
        NumberOfMS = 8,
        MatrixWidth = 256,
        MatrixHeight = 256,
        NumberOfCores = 3
      ),
      new LayerParameters(
        K = 16,
        BiasWidth = 8,
        AccumulatorWidth = 10,
        NumberOfPUs = 10,
        AddressWidth = 0,
        NumberOfMS = 5,
        MatrixWidth = 256,
        MatrixHeight = 10,
        NumberOfCores = 1
      )
    )
    val layers = Range(0, 4).map(i =>
      new LayerData(
        parameters=parametersList(i),
        weights=testData.matrices(i),
        biases=testData.biases(i)
      )
    ).toList

    return layers
  }
}


class Pacman(
  inDataWordWidth: Int,
  val layers: List[LayerData] = PacmanSetup.getLayers
) extends Module {

  val lastLayer = layers.last
  val firstLayer = layers(0)
  val inputCores = firstLayer.parameters.NumberOfCores
  val outputCores = lastLayer.parameters.NumberOfCores
  val netAnswerWidth = lastLayer.parameters.MatrixHeight
  val netInputWordWidth = firstLayer.parameters.K
  val netInputWordPerBlock = firstLayer.parameters.MatrixWidth / firstLayer.parameters.K

  val io = new Bundle {
    val inDataStream = Decoupled(Bits(width=inDataWordWidth)).flip
    val digitOut = Decoupled(UInt(width=log2Up(netAnswerWidth)))
  }

  val widthConverter = Module(new WidthConverter(inDataWordWidth, netInputWordWidth))
  val interleaver = Module(new Interleaver(firstLayer.parameters))
  val net = Module(new Net(layers))
  val deinterleaver = Module(new Deinterleaver(lastLayer.parameters))

  io.inDataStream <> widthConverter.io.wordIn

  widthConverter.io.wordOut <> interleaver.io.wordIn

  net.io.xsIn := interleaver.io.interleavedOut
  net.io.start := interleaver.io.startOut
  interleaver.io.pipeReady := net.io.ready

  deinterleaver.io.doneIn := net.io.done
  deinterleaver.io.oneBitPerCore.bits := net.io.xsOut.reduceLeft(Cat(_, _))
  deinterleaver.io.oneBitPerCore.valid := net.io.xsOutValid
  net.io.pipeReady := deinterleaver.io.oneBitPerCore.ready

  io.digitOut.bits := OHToUInt(deinterleaver.io.oneHotOut.bits)
  io.digitOut.valid := deinterleaver.io.oneHotOut.valid
  deinterleaver.io.oneHotOut.ready := io.digitOut.ready
}
