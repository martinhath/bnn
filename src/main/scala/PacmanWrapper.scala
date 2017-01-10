package Pacman

import Chisel._


class PacmanWrapper() extends Module
{
	val queueSize = 16;
	val io = new Bundle()
	{
		val usb_data = Decoupled(Bits(INPUT, width = 8)).flip
		val net_result = Decoupled(UInt(OUTPUT, width = 16))
		val ulpi_clk = Clock(INPUT)
		val ebiAck = Bool(INPUT)
	}

	io.ulpi_clk.setName("ulpi_clk")
	val AsyncFifo_inst = Module(new AsyncFifo(Bits(width = 8), queueSize, io.ulpi_clk, Driver.implicitClock))
	val Pacman_inst = Module(new Pacman(8))	
	val MCUOutput_inst = Module(new MCUOutput(Pacman_inst.layers(3).parameters.MatrixHeight))

	AsyncFifo_inst.io.enq <> io.usb_data
	Pacman_inst.io.inDataStream <> AsyncFifo_inst.io.deq
	MCUOutput_inst.io.oneHotIn <> Pacman_inst.io.digitOut
	io.net_result <> MCUOutput_inst.io.ebi
	MCUOutput_inst.io.ebiAck := io.ebiAck
}


object PacmanWrapperTest
{
	def main(args: Array[String]): Unit =
	{
		val margs = Array("--backend", "v", "--genHarness", "--compile")
		chiselMain(margs, () => Module(new PacmanWrapper()))
	}
}

