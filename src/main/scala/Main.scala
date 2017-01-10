package Pacman

import Chisel._

// Entry point for the whole project
object Main {
  def main(args: Array[String]) {
    chiselMain(args, () => Module(new PacmanWrapper()))
  }
}
