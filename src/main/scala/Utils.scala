package Pacman

import scala.io.Source

class TestData(
  // 4 Matrices
  val matrices: Array[Array[Array[Int]]],
  // and its biases
  val biases: Array[Array[Int]],
  // 10 sets of 5 vectors of result data
  val vectors: Array[Array[Array[Int]]]
)

object Utils {

  def stringToVector(string: String): Array[Int] = {
    string.split(" ")
          .filter(s => s.length != 0)
          .map(s => {
            val n = Integer.parseInt(s, 10)
            if (n == 1) 1
            else if (n == -1) 0
            else throw new Error("Illegal char %s in string %s".format(s, string))
          })
          .toArray
  }

  def sliceToMatrix(slice: Seq[String]): Array[Array[Int]] = {
    slice.map(stringToVector).toArray
  }

  def printMatrix(mat: Array[Array[Int]]) {
    mat.foreach(line => {
      line.foreach(n => {
        print("%1d ".format(n))
      })
      println()
    })
  }

  def fromOneHot(arr: Array[Int]): Int = {
    arr.indexWhere(_ > 0)
  }

  def readDumpFile() : TestData = {
    // This function only works for the `net_dump` file.
    // Please do not use it for anything else :)
    val lines = Source.fromFile("net_dump").getLines.toArray
    var i = 1
    val firstMatrix = sliceToMatrix(lines.slice(i, i + 256))
    i += 257
    val firstBiases = lines(i).split(" ").filter(s => s.length != 0)
                      .map(s => Integer.parseInt(s, 10)).toArray
    i += 2
    val secondMatrix = sliceToMatrix(lines.slice(i, i + 256))
    i += 257
    val secondBiases = lines(i).split(" ").filter(s => s.length != 0)
                      .map(s => Integer.parseInt(s, 10)).toArray
    i += 2
    val thirdMatrix = sliceToMatrix(lines.slice(i, i + 256))
    i += 257
    val thirdBiases = lines(i).split(" ").filter(s => s.length != 0)
                      .map(s => Integer.parseInt(s, 10)).toArray
    i += 2
    val fourthMatrix = sliceToMatrix(lines.slice(i, i + 10))
    i += 11
    val fourthBiases = lines(i).split(" ").filter(s => s.length != 0)
                      .map(s => Integer.parseInt(s, 10)).toArray
    i += 2

    val NImages = 9

    val NResults = 5
    val results = List.range(0, NImages).map(_ => {
      List.range(0, NResults).map(_ => {
        val v = stringToVector(lines(i))
        i += 2
        v
      }).toArray
    }).toArray

    new TestData(
      Array(firstMatrix, secondMatrix, thirdMatrix, fourthMatrix),
      Array(firstBiases, secondBiases, thirdBiases, fourthBiases),
      results
    )
  }

  def createHostStream(
    inputStreamWordWidth: Int,
    inputK: Int,
    inputCores: Int
  ) {
    val testData = readDumpFile()
    val testInputs = testData.vectors.map(_(0)).take(4).toArray
    val inputVectorSize = testData.vectors(0)(0).length

    val chunksInImage = inputVectorSize / inputK
    val kChunkedTestInputs = testInputs.map(_.grouped(inputK).toArray).toArray
    val warpGrouped = kChunkedTestInputs.grouped(inputCores).map(_.transpose.toArray).toArray
    val netInputWordArrays = warpGrouped.map(_.map(_.flatten.toArray).toArray).flatten.toArray
    val inputStreamWordArrays = netInputWordArrays.flatten.grouped(inputStreamWordWidth).toArray

    val inputStreamInts = inputStreamWordArrays.map(_.zipWithIndex.map{
                                                      case (t, i) => t * Math.pow(2, i)
                                                    }.sum.toInt).toArray


    println(inputStreamInts.deep.mkString("\n"))
  }
}
