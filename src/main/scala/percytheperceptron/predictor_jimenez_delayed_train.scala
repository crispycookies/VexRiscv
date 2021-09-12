package percytheperceptron

import percytheperceptron.ml.perceptron.perceptron
import spinal.core._
import percytheperceptron.memory.shift_register
import percytheperceptron.ml.perceptron.perceptron
import percytheperceptron.ml.trainer.{eicher_trainer, jimenez_trainer}

class predictor_jimenez_delayed_train(bit_width: Int,
                                      feature_count: Int,
                                      table_size: Int,
                                      address_bit_width: Int,
                                      index_bit_width: Int,
                                      delay: Int,
                                      threshold: Int
                                     ) extends Component {
  val io = new Bundle {
    val taken = in UInt (1 bits)
    val prediction = out UInt (1 bits)
    val delayed_prediction = out UInt (1 bits)
    val address = in UInt (address_bit_width bits)
  }

  def lower_bound = -1
  def upper_bound = 1
  def zero = 0

  def map_to_value(taken: UInt): SInt = {
    val scaled = SInt(bit_width bits)

    when(taken === 1) {
      scaled := upper_bound
    } otherwise {
      scaled := lower_bound
    }
    scaled
  }

  def unmap_from_value(taken: SInt): UInt = {
    val scaled = UInt(1 bits)

    when(taken === upper_bound) {
      scaled := 1
    } otherwise {
      scaled := 0
    }
    scaled
  }

  // prediction related
  val ghr = new history_table(bit_width = bit_width, feature_count = feature_count)
  ghr.io.taken := io.taken

  val hash = new mod_index(address_bit_width = address_bit_width, index_bit_width = index_bit_width, table_size = table_size)
  hash.io.address := io.address

  val pt = new weight_table(bit_width = bit_width, feature_count = feature_count, table_size = table_size, address_bit_width = index_bit_width)
  pt.io.address_read := hash.io.index

  val comb_perceptron = new perceptron(bit_width = bit_width, feature_count = feature_count, lower_bound = lower_bound, upper_bound = upper_bound, zero = zero)
  comb_perceptron.io.bias := pt.io.bias_out
  comb_perceptron.io.weights := pt.io.weights_out

  for (i <- 0 until feature_count) {
    comb_perceptron.io.values(i) := map_to_value(ghr.io.history(i).asUInt)
  }


  // delay related
  val delay_by_n = new delay_unit(bit_width = bit_width, feature_count = feature_count, delay = delay, index_bit_width = index_bit_width)
  delay_by_n.io.inbias := pt.io.bias_out
  delay_by_n.io.inweights := pt.io.weights_out
  delay_by_n.io.inghr := ghr.io.history
  delay_by_n.io.inpred := comb_perceptron.io.prediction
  delay_by_n.io.inpred_num := comb_perceptron.io.prediction_numeric
  delay_by_n.io.inaddress := hash.io.index

  // training related
  val trainer = new jimenez_trainer(bit_width = bit_width, feature_count = feature_count, threshold = threshold)
  trainer.io.predicted := delay_by_n.io.outpred
  trainer.io.predicted_numerical := delay_by_n.io.outpred_num
  trainer.io.actual := map_to_value(io.taken)
  trainer.io.bias_old := delay_by_n.io.outbias
  trainer.io.current_weights := delay_by_n.io.outweights
  trainer.io.current_data := delay_by_n.io.outghr

  // write back
  pt.io.address_write := delay_by_n.io.outaddress
  pt.io.weights_in := trainer.io.new_weigths
  pt.io.bias_in := trainer.io.bias_new

  // output
  io.prediction := unmap_from_value(comb_perceptron.io.prediction)
  io.delayed_prediction := unmap_from_value(delay_by_n.io.outpred)
}

