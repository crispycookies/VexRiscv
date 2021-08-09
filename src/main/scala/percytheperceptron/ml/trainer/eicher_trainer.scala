package percytheperceptron.ml.trainer

import spinal.core._
import spinal.lib._

class eicher_trainer(bit_width: Int, feature_count: Int, threshold: Int) extends Component {
  val io = new Bundle {
    val bias_new: SInt = out SInt (bit_width bits)
    val bias_old: SInt = in SInt (bit_width bits)
    val current_weights: Vec[SInt] = in Vec(SInt(bit_width bits), feature_count)
    val new_weigths: Vec[SInt] = out Vec(SInt(bit_width bits), feature_count)
    val current_data = in Vec(SInt(bit_width bits), feature_count)
    val predicted: SInt = in SInt (bit_width bits)
    val predicted_numerical: SInt = in SInt (bit_width bits)
    val actual: SInt = in SInt (bit_width bits)
  }
  when(io.bias_old < threshold && io.bias_old > -threshold) {
    when(io.actual === 1) {
      io.bias_new := io.bias_old + 1
    } otherwise {
      io.bias_new := io.bias_old - 1
    }
  } otherwise {
    when(io.bias_old > threshold) {
      io.bias_new := threshold
    } otherwise {
      io.bias_new := -threshold
    }
  }
  when(io.predicted_numerical < threshold && io.predicted_numerical > threshold) {
    for (i <- 0 until feature_count) {
      when(io.actual === io.current_data(i)) {
        when(io.current_weights(i) >= threshold) {
          io.new_weigths(i) := threshold
        } otherwise {
          io.new_weigths(i) := io.current_weights(i) + 1
        }
      } otherwise {
        when(io.current_weights(i) <= -threshold) {
          io.new_weigths(i) := -threshold
        } otherwise {
          io.new_weigths(i) := io.current_weights(i) - 1
        }
      }
    }
  } otherwise {
    io.new_weigths := io.current_weights
  }
}

