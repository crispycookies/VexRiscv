package percytheperceptron.ml.perceptron.util

import spinal.core._
import spinal.lib._

class activation(bit_width : Int, lower_bound: Int, upper_bound : Int, zero : Int) extends Component {
  val io = new Bundle {
    val trigger: SInt = in SInt(bit_width bits)
    val step: SInt = out SInt (bit_width bits)
  }
  when(io.trigger > zero) {
    io.step := upper_bound
  } otherwise {
    io.step := lower_bound
  }
}

