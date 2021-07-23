package percytheperceptron.ml.trainer

import percytheperceptron.ml.trainer.util.get_update
import percytheperceptron.ml.trainer.util.math.{vector_add, vector_mul}
import spinal.core._

class trainer(bit_width: Int, feature_count: Int) extends Component {
  val io = new Bundle {
    val bias: SInt = out SInt(bit_width bits)
    val current_weights: Vec[SInt] = in Vec(SInt(bit_width bits), feature_count)
    val new_weigths: Vec[SInt] = out Vec(SInt(bit_width bits), feature_count)
    val current_data = in Vec(SInt(bit_width bits), feature_count)
    val predicted: SInt = in SInt(bit_width bits)
    val eta: SInt = in SInt(bit_width bits)
    val actual: SInt = in SInt(bit_width bits)
  }
  // error = actual - predicted
  // update = eta * error
  val update_instance = new get_update(bit_width = bit_width)
  update_instance.io.actual := io.actual
  update_instance.io.predicted := io.predicted
  update_instance.io.eta := io.eta

  // assign result onto update
  val update: SInt = SInt(bit_width bits)
  update := update_instance.io.y

  /*
  * ---------------------------------------------------------
  * ---------------------------------------------------------
  */

  // for each input : update * input[i]
  val weight_mul_instance = new vector_mul(bit_width = bit_width, feature_count = feature_count)
  weight_mul_instance.io.a := io.current_data
  weight_mul_instance.io.b := update




  val multiplied: Vec[SInt] = Vec(SInt(bit_width bits), feature_count)
  multiplied := weight_mul_instance.io.y

  // for each weight: (old) weight + muled
  val weight_add_instance = new vector_add(bit_width = bit_width, feature_count = feature_count)
  weight_add_instance.io.a := io.current_weights
  weight_add_instance.io.b := multiplied

  val new_weights: Vec[SInt] = Vec(SInt(bit_width bits), feature_count)
  new_weights := weight_add_instance.io.y

  // write back
  io.bias := update
  io.new_weigths := new_weights
}

