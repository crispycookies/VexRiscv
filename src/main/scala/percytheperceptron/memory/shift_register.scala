package percytheperceptron.memory

import spinal.core._
import spinal.lib._

class shift_register(bit_width : Int, depth : Int) extends Component {
  val io = new Bundle {
    val input = in SInt(bit_width bits)
    val output = out SInt (bit_width bits)
  }
    val register = Vec(Reg(SInt(bit_width bits)) init(0), depth)
    register(0) := io.input

    for (i <- 1 until depth) {
      register(i) := register(i-1)
    }
    io.output := register(depth - 1)
}

