package percytheperceptron.memory.util

import spinal.core._

class file(bit_width : Int, row_count : Int, sub_cell_count : Int) extends Component {
  val io = new Bundle {
    val features_to_store = in Vec(Vec(UInt(bit_width bits),sub_cell_count),row_count)
    val features_from_store = out Vec(Vec(UInt(bit_width bits),sub_cell_count),row_count)
    val write_ena = in Vec(UInt(1 bits), row_count)
  }

  val memory_cells = Array.fill(row_count)(new cell(bit_width=bit_width, sub_cell_count = sub_cell_count))

  for (i <- 0 until row_count){
    io.features_from_store(i) := memory_cells(i).io.features_from_store
    memory_cells(i).io.features_to_store := io.features_to_store(i)
    memory_cells(i).io.write_ena := io.write_ena(i)
  }
}
