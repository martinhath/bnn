package Pacman

import Chisel._

class Counter(start: Int, max: Int, step: Int = 1) extends Module {
  val io = new Bundle {
    val enable = Bool().asInput
    val rst = Bool().asInput
    val value = UInt().asOutput
  }

  val startValue = UInt(start, width=UInt(max - 1).getWidth)
  val v = Reg(init = startValue)
  when(io.enable) {
    when(io.rst) {
      v := UInt(start)
    }.otherwise {
      v := v + UInt(step)
    }
  }
  io.value := v
}

class UpDownCounter(start: Int, end: Int, upStep: Int = 1, downStep: Int = 1) extends Module {
  val io = new Bundle {
    val up    = Bool().asInput
    val down  = Bool().asInput
    val value = UInt().asOutput
  }
  val reg = Reg(init = UInt(start, width=UInt(end).getWidth))
  when (io.up && io.down) {
  } .elsewhen (io.up) {
    reg := reg + UInt(upStep)
  } .elsewhen (io.down) {
    reg := reg - UInt(downStep)
  }
  io.value := reg
}


class CounterWithSyncAndAsyncReset(start: Int, max: Int, step: Int = 1) extends Module {
  val io = new Bundle {
    val enable = Bool().asInput
    val syncRst = Bool().asInput
    val asyncRst = Bool().asInput
    val value = UInt().asOutput
  }

  val startValue = UInt(start, width=UInt(max - 1).getWidth)
  val v = Reg(init = startValue)

  when(io.enable) {
    when(io.asyncRst) {
      v := UInt(start + step)
    }.elsewhen(io.syncRst) {
      v := UInt(start)
    }.otherwise {
      v := v + UInt(step)
    }
    io.value := Mux(io.asyncRst, UInt(start), v)
  }.otherwise {
    io.value := v
  }
}

class AsyncCounter(start: Int, end: Int, step: Int = 1) extends Module {
  if((end - start) % step != 0) {
    throw new AssertionError("Step size is not a divisor of (end - start)")
  }

  val io = new Bundle {
    val enable = Bool().asInput
    val value = UInt().asOutput
  }

  val startValue = UInt(start, width=UInt(end - 1).getWidth)
  val v = Reg(init=UInt(startValue))

  when(io.enable) {
    val nextValue = Mux(v === UInt(end - step), UInt(start), v + UInt(step))
    v := nextValue
    io.value := nextValue
  }.otherwise {
    io.value := v
  }
}

class AsyncUpDownCounter(start: Int, end: Int, step: Int = 1) extends Module {
  if((end - start) % step != 0) {
    throw new AssertionError("Step size is not a divisor of (end - start)")
  }

  val io = new Bundle {
    val up    = Bool().asInput
    val down  = Bool().asInput
    val value = UInt().asOutput
  }
  val reg = Reg(init = UInt(start, width=UInt(end).getWidth))
  when (io.up && io.down) {
    io.value := reg + UInt(step)
    reg := reg
  } .elsewhen (io.up) {
    val v = reg + UInt(step)
    reg := v
    io.value := v
  } .elsewhen (io.down) {
    val v = reg - UInt(step)
    reg := v
    io.value := reg
  } .otherwise {
    io.value := reg
    reg := reg
  }
}

class WrappingCounter(start: Int, end: Int, step: Int = 1) extends Module {
  val io = new Bundle {
    val enable = Bool().asInput
    val value = UInt().asOutput
  }

  if (end == step || step == 0) {
    io.value := UInt(start)
  } else {
    val startValue = UInt(start, width=UInt(end + step).getWidth)
    val v = Reg(init = startValue)
    when(io.enable) {
      // v := (v + UInt(step)) % UInt(end)
      when(v + UInt(step) >= UInt(end)) {
        v := (v + UInt(step)) - UInt(end)
      }.otherwise{
        v := v + UInt(step)
      }
    }
    io.value := v
  }
}

class CounterWithNonBlockingReset(start: Int, max: Int, step: Int = 1) extends Module {
  val io = new Bundle {
    val enable = Bool().asInput
    val rst = Bool().asInput
    val value = UInt().asOutput
  }

  val startValue = UInt(start, width=UInt(max - 1).getWidth)
  val reg = Reg(init = startValue)
  val base = Mux(io.rst, startValue, reg)
  when(io.enable) {
    reg := UInt(base) + UInt(step)
  }.otherwise {
    reg := base
  }
  io.value := reg
}
