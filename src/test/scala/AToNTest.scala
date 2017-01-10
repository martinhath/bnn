package Pacman

import Chisel._

class AToNTests(c: AToN, width_in : Int, width_out : Int) extends Tester(c)
{
	println("Testing with width in:",width_in,"and width out:",width_out)
	if(width_in == width_out)
	{
		poke(c.io.a.bits,0)
		poke(c.io.n.ready,false)
		poke(c.io.a.valid,false)
		step(1)
		expect(c.io.n.bits,0)
		expect(c.io.a.ready,false)
		expect(c.io.n.valid,false)
		step(1)
		poke(c.io.a.bits,0x3f)
		poke(c.io.a.valid,false)
		poke(c.io.n.ready,false)
		expect(c.io.a.ready,false)
		expect(c.io.n.valid,false)
		expect(c.io.n.bits,0x3f)
		step(1)
	
		poke(c.io.a.bits,0xaf)
		poke(c.io.a.valid,true)
		poke(c.io.n.ready,false)
		expect(c.io.n.valid,true)
		expect(c.io.a.ready,false)
		expect(c.io.n.bits,0xaf)
		step(1)

		poke(c.io.a.bits,0x01)
		poke(c.io.a.valid,false)
		poke(c.io.n.ready,true)
		expect(c.io.n.bits,0x01)
		expect(c.io.n.valid,false)
		expect(c.io.a.ready,true)
		step(1)
	}
	else if(width_in < width_out)
	{
		poke(c.io.a.bits,0)
		poke(c.io.a.valid,false)
		poke(c.io.n.ready,false)
		step(1)
		expect(c.io.n.bits,0)
		expect(c.io.n.valid,false)
		expect(c.io.a.ready,true)
		step(1)

		poke(c.io.n.ready,true)
		step(1)
		expect(c.io.n.bits,0)
		expect(c.io.a.ready,true)
		expect(c.io.n.valid,false)		
		step(1)
		var inputVal = 0x445d27a
		var inputMask = 0
		var outputMask = 0
		for (i <- 0 until width_in){
			inputMask = inputMask | (1 << i)
		}
		for (i <- 0 until width_out){
			outputMask = outputMask | (1 << i)
		}
		
		for(i <- 0 until width_out/width_in)
		{
			val sendVal = (inputVal >>> (i * width_in)) & inputMask
			poke(c.io.a.bits,sendVal)
			step(1)
			expect(c.io.a.ready,true)
			expect(c.io.n.valid,false)
			step(1)
			poke(c.io.a.valid,true)
			step(1)
			if(i == (width_out/width_in) - 1)
			{
				expect(c.io.a.ready,false)
				expect(c.io.n.valid,true)
				expect(c.io.n.bits,inputVal & outputMask)
				poke(c.io.a.valid, false)
				step(1)
				expect(c.io.a.ready,true)
				expect(c.io.n.valid,false)
				step(1)
			}
			else
			{
				expect(c.io.a.ready,true)
				expect(c.io.n.valid,false)
				poke(c.io.a.valid,false)
				step(1)
			}
		}
		
		inputVal = 0x257dc0
		for(i <- 0 until width_out/width_in)
		{
			val sendVal = (inputVal >>> (i * width_in)) & inputMask
			poke(c.io.a.bits,sendVal)
			step(1)
			expect(c.io.a.ready,true)
			expect(c.io.n.valid,false)
			step(1)
			poke(c.io.a.valid,true)
			step(1)
			if(i == (width_out/width_in) - 1)
			{
				expect(c.io.a.ready,false)
				expect(c.io.n.valid,true)
				expect(c.io.n.bits,inputVal & outputMask)
				poke(c.io.a.valid, false)
				step(1)
				expect(c.io.a.ready,true)
				expect(c.io.n.valid,false)
				step(1)
			}
			else
			{
				expect(c.io.a.ready,true)
				expect(c.io.n.valid,false)
				poke(c.io.a.valid,false)
				step(1)
			}
		}
	}
	else
	{
		poke(c.io.a.bits,0)
		poke(c.io.n.ready,false)
		poke(c.io.a.valid,false)
		step(1)
		expect(c.io.n.bits,0)
		expect(c.io.a.ready,true)
		expect(c.io.n.valid,false)
		step(1)

		poke(c.io.n.ready,true)
		step(1)
		expect(c.io.n.bits,0)
		expect(c.io.a.ready,true)
		expect(c.io.n.valid,false)		
		step(2)
		var inputVal = 0xb87c86
		var outputMask = 0
		var inputMask = 0
		for (i <- 0 until width_out){
			outputMask = outputMask | (1 << i)
		}
		for (i <- 0 until width_in){
			inputMask = inputMask | (1 << i)
		}

		poke(c.io.a.bits, inputVal & inputMask)
		step(1)
		expect(c.io.a.ready,true)
		expect(c.io.n.valid,false)
		step(1)
		poke(c.io.a.valid,true)
		step(1)
		poke(c.io.a.valid,false)
		expect(c.io.n.bits,inputVal & outputMask)
		expect(c.io.a.ready,false)
		expect(c.io.n.valid,true)
		for (i <- 1 until width_in/width_out)
		{
			step(1)
			println(i*width_in, " # ",outputMask)
			expect(c.io.n.bits,(inputVal >>> (i*width_out)) & outputMask)
			expect(c.io.n.valid,true)
			expect(c.io.a.ready,false)			
		}
		step(1)
		expect(c.io.a.ready, true)
		expect(c.io.n.valid, false)

		
		inputVal = 0x56b424

		poke(c.io.a.bits, inputVal & inputMask)
		step(1)
		expect(c.io.a.ready,true)
		expect(c.io.n.valid,false)
		step(1)
		poke(c.io.a.valid,true)
		step(1)
		expect(c.io.n.bits,inputVal & outputMask)
		expect(c.io.a.ready,false)
		expect(c.io.n.valid,true)
		for (i <- 1 until width_in/width_out)
		{
			step(1)
			println(i*width_in, " # ",outputMask)
			expect(c.io.n.bits,(inputVal >>> (i*width_out)) & outputMask)
			expect(c.io.n.valid,true)
			expect(c.io.a.ready,false)			
		}
		step(1)
		expect(c.io.a.ready, true)
		expect(c.io.n.valid, false)
	}
	
}

object AToNTest
{
	def test(width_in: Int, width_out : Int)
	{
		val margs = Array("--backend", "c", "--genHarness","--compile"/*,"--test"*/)
		
		chiselMainTest(margs,() => Module(new AToN(width_in, width_out)))
		{
			c => new AToNTests(c, width_in, width_out)
		}
		println("Done with this test")
	}
	def main(args: Array[String]): Unit =
	{
		println("Testing equal size on input and output")
		test(3,3)
		test(8,8)

		println("Testing width in smaller than width out");
		test(3,9)
		test(4,8)

		println("Testing width in greater than width out")
		test(9,3)
		test(8,4)

		println("Generating the actual case")
		test(8,72)
	}
}
