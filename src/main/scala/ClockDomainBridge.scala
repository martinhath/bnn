package Pacman

import Chisel._



class ClockDomainBridge() extends Module
{
	val queueSize = 16
	val io = new Bundle
	{
		val usb_data = Decoupled(Bits(INPUT, width = 8)).flip
		val net_data = Decoupled(Bits(OUTPUT, width = 32))
		val ulpi_clk = Clock(INPUT)
	}
	io.ulpi_clk.setName("ulpi_clk")

	val AToN_inst = Module(new AToN(8,32))
	val AsyncFifo_inst = Module(new AsyncFifo(Bits(width = 8), queueSize, io.ulpi_clk, Driver.implicitClock))
	
	AsyncFifo_inst.io.enq <> io.usb_data
	AToN_inst.io.a <> AsyncFifo_inst.io.deq
	io.net_data <> AToN_inst.io.n
	
}

object ClockDomainBridgeTest
{
	def main(args: Array[String]): Unit =
	{
		val margs = Array("--backend", "v", "--genHarness","--compile")
		chiselMain(margs,() => Module(new ClockDomainBridge()))
	}
}

