package Pacman

import util.Random

object Matrix {
  def getWeightMatrix(matWidth: Int, matHeight: Int) : Seq[Int] =
    Seq.fill(matWidth * matHeight)(Random.nextInt(2))

  def getInputVector(len: Int): Seq[Int] =
    Seq.fill(len)(Random.nextInt(2))

  def matrixMul(matrix: Array[Array[Int]], vector: Seq[Int]) : Seq[Int] = {
      matrixMul(matrix.flatMap(e => e), vector, matrix(0).length, matrix.length)
  }

  def matrixMul(matrix: Seq[Int], vector: Seq[Int],
                matWidth: Int, matHeight: Int)
      : Seq[Int] = {
    var result = new Array[Int](matHeight)
    for (y <- 0 until matHeight) {
      val i = y * matWidth + matHeight
      var sum = 0
      for (z <- 0 until matWidth) {
        val w_i = y * matWidth + z
        val x_i = z
        sum += (if (matrix(w_i) == vector(x_i)) 1 else 0)
      }
      result(y) = sum
    }
    result
  }

  def printMat(mat: Array[Array[Int]]) {
    printMat(mat.flatMap(id => id), mat.length, mat(0).length)
  }

  def printMat(mat: Seq[Int], h: Int, w: Int) {
    for (y <- 0 until h) {
      for (z <- 0 until w) {
        val i = y * w + z
        print(mat(i))
        print(" ")
      }
      print("\n")
    }
    print("\n")
  }
}
