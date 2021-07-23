package percytheperceptron.ml.trainer.util.math

import spinal.core._
import spinal.lib._

class vector_add(bit_width: Int, feature_count: Int) extends Component {
  val io = new Bundle {
    val a: Vec[SInt] = in Vec(SInt(bit_width bits), feature_count)
    val b: Vec[SInt] = in Vec(SInt(bit_width bits), feature_count)
    val y = out Vec(SInt(bit_width bits), feature_count)
  }
  val weight_product_truncd: Vec[SInt] = Vec(SInt(bit_width bits), feature_count)

  for (i <- 0 until feature_count) {
    weight_product_truncd(i) := io.a(i) + io.b(i)
  }
  io.y := weight_product_truncd
}

