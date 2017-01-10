package Pacman 

import scala.language.reflectiveCalls
import Chisel._

class AToN(width_in : Int, width_out : Int) extends Module
{
	val io = new Bundle
	{
		val a = Decoupled(Bits(width=width_in)).flip
		val n = Decoupled(Bits(width=width_out))
	}
	if(width_in == width_out)
	{
		io.n.bits := io.a.bits
		io.n.valid := io.a.valid
		io.a.ready := io.n.ready
	}
	else if(width_in < width_out)
	{
		var number_of_registers = width_out / width_in
		val regs = Array.fill(number_of_registers)(Reg(init=Bits(0,width_in)))
		val cnt_reg = Reg(init = UInt(0,log2Up(number_of_registers+1)))
		val cnt_reg_incremented = cnt_reg + UInt(1)
		val last_input = cnt_reg === UInt(number_of_registers)
		val a_value = ~last_input
		val valid_a_trans = io.a.valid && a_value
		val valid_n_trans = io.n.ready && last_input
		when (valid_n_trans) {
			cnt_reg := UInt(0)
		}
		when(valid_a_trans)
		{
			cnt_reg := cnt_reg_incremented
		}
		when(valid_a_trans){
			regs.zip(regs.drop(1))
			.foreach({
				case (a, b) => {
					b := a
				}
      			})
			regs(0) := io.a.bits
		}
		io.n.bits := Cat(regs)

		io.a.ready := a_value
		io.n.valid := last_input
	}
	else
	{
		var number_of_registers = width_in / width_out
		var regs=Vec.fill(number_of_registers)(Reg(init=Bits(0,width_out)))
		val cnt_reg = Reg(init = UInt(number_of_registers,log2Up(number_of_registers+1)))
		val cnt_incremented = cnt_reg + UInt(1)
		val last_output = cnt_reg === UInt(number_of_registers)
		val n_val = ~last_output
		val valid_a_trans = io.a.valid && last_output
		val valid_n_trans = io.n.ready && n_val

		when(valid_n_trans)
		{
			cnt_reg := cnt_incremented
		}
		when(valid_a_trans)
		{
			cnt_reg := UInt(0)
		}

		for(i <- 0 until number_of_registers)
		{
			when(valid_a_trans)
			{
				regs(i) := io.a.bits((i+1)*width_out - 1, i*width_out)
			}
		}

		io.n.bits := regs(cnt_reg)
		io.a.ready := last_output
		io.n.valid := n_val
	}
}
