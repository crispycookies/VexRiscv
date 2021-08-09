package percytheperceptron

import percytheperceptron.memory.shift_register
import spinal.core._

class delay_unit(bit_width: Int, feature_count: Int, delay : Int, index_bit_width : Int) extends Component {
  val io = new Bundle {
    val inweights = in Vec(SInt(bit_width bits), feature_count)
    val inbias = in SInt(bit_width bits)
    val outbias = out SInt(bit_width bits)
    val outweights = out Vec(SInt(bit_width bits), feature_count)

    val inpred = in SInt(bit_width bits)
    val outpred = out SInt(bit_width bits)

    val inpred_num = in SInt(bit_width bits)
    val outpred_num = out SInt(bit_width bits)

    val inaddress = in UInt(index_bit_width bits)
    val outaddress = out UInt(index_bit_width bits)

    val inghr = in Vec(SInt(bit_width bits), feature_count)
    val outghr = out Vec(SInt(bit_width bits), feature_count)
  }
  // Delay Weights and Bias by n-cycles
  val perceptron_delay = new delay_perceptron(bit_width = bit_width, feature_count = feature_count, delay = delay)
  perceptron_delay.io.inbias := io.inbias
  perceptron_delay.io.inweights := io.inweights
  io.outweights := perceptron_delay.io.outweights
  io.outbias := perceptron_delay.io.outbias

  // Delay the prediction by n-cycles
  val prediction_delay = new shift_register(bit_width = bit_width, depth = delay)
  prediction_delay.io.input := io.inpred
  io.outpred := prediction_delay.io.output

  val prediction_delay_num = new shift_register(bit_width = bit_width, depth = delay)
  prediction_delay_num.io.input := io.inpred_num
  io.outpred_num := prediction_delay_num.io.output

  // Delay the address by n-cycles
  val address_delay = new shift_register(bit_width = index_bit_width, depth = delay)
  address_delay.io.input := io.inaddress.asSInt
  io.outaddress := address_delay.io.output.asUInt

  // Delay GHR by n-cycles
  val ghr_delay = new delay_vector(bit_width = bit_width, feature_count = feature_count, delay = delay)
  ghr_delay.io.invect := io.inghr
  io.outghr := ghr_delay.io.outvect
}