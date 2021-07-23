package percytheperceptron.ml.perceptron.util

import spinal.core._


class perceptron_core(bit_width: Int, feature_count: Int) extends Component {
  val io = new Bundle {
    val bias: SInt = in SInt(bit_width bits)
    val weights: Vec[SInt] = in Vec(SInt(bit_width bits), feature_count)
    val values: Vec[SInt] = in Vec(SInt(bit_width bits), feature_count)
    val out_data: SInt = out SInt(bit_width bits)
  }
  val delay_sync_bias  = SInt(bit_width bits)
  delay_sync_bias := io.bias

  val nodes_entities: Array[node] = Array.fill(feature_count)(new node(bit_width=bit_width))
  val nodes_result: Vec[SInt] = Vec(SInt(bit_width bits), feature_count)
  val reduce_results: SInt = SInt(bit_width bits)
  val reduce_results_added_bias: SInt = SInt(bit_width bits)

  for (i <- 0 until feature_count){
    nodes_entities(i).io.weight :=  io.weights(i)
    nodes_entities(i).io.value := io.values(i)
    nodes_result(i) := nodes_entities(i).io.out_product
  }
  reduce_results := nodes_result.reduce(_+_)
  reduce_results_added_bias := reduce_results + delay_sync_bias
  io.out_data := reduce_results_added_bias
}

