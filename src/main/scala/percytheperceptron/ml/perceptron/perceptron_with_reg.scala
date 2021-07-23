package percytheperceptron.ml.perceptron

import spinal.core._
import spinal.lib._

class perceptron_with_reg(bit_width: Int, feature_count: Int, lower_bound: Int, upper_bound : Int, zero : Int) extends Component {
  val io = new Bundle {
    val bias: SInt = in SInt(bit_width bits)
    val weights: Vec[SInt] = in Vec(SInt(bit_width bits), feature_count)
    val values: Vec[SInt] = in Vec(SInt(bit_width bits), feature_count)
    val prediction: SInt = out SInt(bit_width bits)
  }

  val bias = RegNext(io.bias)
  val weights = RegNext(io.weights)
  val values = RegNext(io.values)

  val percy = new perceptron(bit_width , feature_count, lower_bound, upper_bound, zero)
  percy.io.bias := bias
  percy.io.weights := weights
  percy.io.values := values

  io.prediction := percy.io.prediction;

}

