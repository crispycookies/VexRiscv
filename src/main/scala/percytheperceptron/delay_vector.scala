package percytheperceptron

import percytheperceptron.memory.shift_register
import spinal.core._

class delay_vector(bit_width: Int, feature_count: Int, delay : Int) extends Component {
  val io = new Bundle {
    val invect = in Vec(SInt(bit_width bits), feature_count)
    val outvect = out Vec(SInt(bit_width bits), feature_count)
  }
  val nodes_entities: Array[shift_register] = Array.fill(feature_count)(new shift_register(bit_width=bit_width, depth = delay))
  for (i <- 0 until feature_count) {
    nodes_entities(i).io.input := io.invect(i)
    io.outvect(i) := nodes_entities(i).io.output
  }
}
