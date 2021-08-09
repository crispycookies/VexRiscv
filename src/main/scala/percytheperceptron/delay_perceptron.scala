package percytheperceptron

import percytheperceptron.memory.shift_register
import spinal.core._

class delay_perceptron(bit_width: Int, feature_count: Int, delay : Int) extends Component{
  val io = new Bundle {
    val inweights = in Vec(SInt(bit_width bits), feature_count)
    val inbias = in SInt(bit_width bits)
    val outbias = out SInt(bit_width bits)
    val outweights = out Vec(SInt(bit_width bits), feature_count)
  }
  val weight_reg = new delay_vector(bit_width, feature_count, delay)
  weight_reg.io.invect := io.inweights
  io.outweights := weight_reg.io.outvect

  val bias_reg = new shift_register(bit_width, delay)
  bias_reg.io.input := io.inbias
  io.outbias := bias_reg.io.output
}
