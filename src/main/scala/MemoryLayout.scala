package Pacman

object MemoryLayout {
  private[this] def rotate(seq: IndexedSeq[Int], steps: Int): IndexedSeq[Int] = {
    seq.takeRight(steps) ++ seq.dropRight(steps)
  }
  private[this] def paddedBinary(x: Int, width: Int): String = {
    val all = ("%" + width + "s").format(Integer.toBinaryString(x)).replace(' ', '0')
    all.slice(all.length - width, all.length)
  }

  def getStreams(parameters: LayerParameters,
    weights: Array[Array[Int]],
    bias: Array[Int]): (Array[Array[String]], Array[String]) = {

    val PUsPerMUs = parameters.NumberOfPUs / parameters.NumberOfMS

    assert(weights.length == parameters.MatrixHeight,
      "Parameters doesn't match matrix dimensions")
    assert(weights(0).length == parameters.MatrixWidth,
      "Parameters doesn't match matrix dimensions")

    assert(weights.length % parameters.NumberOfPUs == 0,
      "The number of PUs have to be a divisor of matrixHeight")
    assert(weights(0).length % parameters.K == 0,
      "K has to be divisor of matrixWidth")

    assert(parameters.NumberOfPUs % parameters.NumberOfMS == 0,
      "NumberOfMUs has to be a divisor of NumberOfPUs")

    val unshifted_PUstreams = Range(0, parameters.NumberOfPUs)
      .map(pui => weights.zipWithIndex
        .filter({ case (row, i) => i % parameters.NumberOfPUs == pui })
        .flatMap(pair => pair._1))

    val shifted_PUstreams = unshifted_PUstreams
      .zipWithIndex.map({ case (s, i) => rotate(s, parameters.K * (i % PUsPerMUs)) })

    val PU_word_streams = shifted_PUstreams
      .map(s => s.grouped(parameters.K).toList)

    val MU_seperated = PU_word_streams
      .grouped(PUsPerMUs)
      .map(_.transpose)
      .toList

    val MU_word_streams = MU_seperated
      .map(_.map(_.reverse
        .flatMap(_.reverse)
        .mkString).map("b" + _))

    val weightStreams = MU_word_streams.map(_.toArray).toArray

    val biasStream = bias
      .grouped(parameters.NumberOfPUs)
      .flatMap(_ ++ List.fill(parameters.MatrixWidth / parameters.K - parameters.NumberOfPUs)(0))
      .map("b" + paddedBinary(_, parameters.BiasWidth)).toArray

    return (weightStreams, biasStream)
  }
}

