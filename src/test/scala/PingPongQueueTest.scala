package Pacman

import Chisel._

class PingPongQueueTests(c: PingPongQueue, data_width : Int, queue_size : Int) extends Tester(c)
{
	var testInput = List.range(queue_size, 0, -1)
	
	poke(c.io.valid_trans, false)
	poke(c.io.write_en, true)
	poke(c.io.data_in, 0)
	poke(c.io.reset, false)
	step(1)
	expect(c.io.is_full, false)
	
	//load values into queue
	for(i <- 0 until testInput.length)
	{
		val testVal = testInput(i)
		poke(c.io.data_in, testVal)
		poke(c.io.valid_trans, true)

		if(i < testInput.length - 1)
		{
			expect(c.io.is_full, false)
		}
		else
		{
			expect(c.io.is_full, true)
		}
		step(1)
		poke(c.io.valid_trans, false)
		step(1)
	}

	//now read the data some times
	poke(c.io.write_en, false)
	for (i <- 1 to  5)
	{
		for(testVal <- testInput)
		{
			expect(c.io.data_out, testVal)
			step(1)
		}
	}
	step(3)

	//now run another test to see if it is able to reset and fill again
	testInput = List.range(queue_size * 2, queue_size, -1)
	poke(c.io.valid_trans, false)
	poke(c.io.write_en, true)
	poke(c.io.data_in, 0)
	poke(c.io.reset, true)
	step(1)
	expect(c.io.is_full, false)
	poke(c.io.valid_trans, true)
	poke(c.io.reset, false)
	for(i <- 0 until testInput.length)
	{
		val testVal = testInput(i)
		poke(c.io.data_in, testVal)
		poke(c.io.valid_trans,true)
		if(i < testInput.length - 1)
		{
			expect(c.io.is_full, false)
		}
		else
		{
			expect(c.io.is_full, true)
		}
		step(1)
		poke(c.io.valid_trans, false)
		step(1)
	}

	//now test the data again for some iterations
	poke(c.io.write_en, false)
	for (i <- 1 to 5)
	{
		for(testVal <- testInput)
		{
			expect(c.io.data_out, testVal)
			step(1)
		}
	}
}

object PingPongQueueTest
{
	def main(args: Array[String])
	{
		var data_width = 36
		var queue_size = 4
		val margs = Array("--backend", "c", "--genHarness", "--compile", "--test")
		chiselMainTest(margs, () => Module(new PingPongQueue(data_width, queue_size)))
		{
			c => new PingPongQueueTests(c, data_width, queue_size)
		}
	}
}

