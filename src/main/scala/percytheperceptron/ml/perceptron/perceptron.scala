package percytheperceptron.ml.perceptron

import percytheperceptron.ml.perceptron.util.{activation, perceptron_core}
import spinal.core._

class perceptron(bit_width: Int, feature_count: Int, lower_bound: Int, upper_bound : Int, zero : Int) extends Component {
  val io = new Bundle {
    val bias: SInt = in SInt(bit_width bits)
    val weights: Vec[SInt] = in Vec(SInt(bit_width bits), feature_count)
    val values: Vec[SInt] = in Vec(SInt(bit_width bits), feature_count)
    val prediction: SInt = out SInt(bit_width bits)
    val prediction_numeric : SInt = out SInt(bit_width bits)
  }
  val percy = new perceptron_core(bit_width = bit_width, feature_count = feature_count)
  val activation_function = new activation(bit_width = bit_width, lower_bound = lower_bound, upper_bound = upper_bound, zero = zero);

  percy.io.weights := io.weights
  percy.io.values := io.values
  percy.io.bias := io.bias

  activation_function.io.trigger := percy.io.out_data
  io.prediction := activation_function.io.step
  io.prediction_numeric := percy.io.out_data
}

