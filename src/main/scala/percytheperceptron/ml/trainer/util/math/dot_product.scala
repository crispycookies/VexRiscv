package percytheperceptron.ml.trainer.util.math

import spinal.core._
import spinal.lib._

class dot_product(bit_width: Int, feature_count: Int) extends Component {
  val io = new Bundle {
    val a: Vec[SInt] = in Vec(SInt(bit_width bits), feature_count)
    val b: Vec[SInt] = in Vec(SInt(bit_width bits), feature_count)
    val res = out SInt(bit_width bits)
  }
  val dot_products: Vec[SInt] = Vec(SInt(bit_width*2 bits), feature_count)
  val dot_products_truncd: Vec[SInt] = Vec(SInt(bit_width bits), feature_count)

  for (i <- 0 until feature_count) {
    dot_products(i) := io.a(i) * io.b(i)
  }
  for (i <- 0 until feature_count) {
    dot_products_truncd(i) := dot_products(i)(bit_width-1 downto 0)
  }
  io.res := dot_products_truncd.reduce(_+_)
}

