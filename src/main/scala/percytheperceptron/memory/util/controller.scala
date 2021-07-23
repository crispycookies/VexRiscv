package percytheperceptron.memory.util

import spinal.core._

class controller(address_bit_width : Int, bit_width : Int, row_count : Int, sub_cell_count : Int) extends Component{
  val io = new Bundle {
    val features_to_store = out Vec(Vec(UInt(bit_width bits),sub_cell_count),row_count)
    val features_from_store = in Vec(Vec(UInt(bit_width bits),sub_cell_count),row_count)
    val write_ena = out Vec(UInt(1 bits), row_count)

    val address_write = in UInt(address_bit_width bits)
    val address_read = in UInt(address_bit_width bits)
    val write_ena_user = in UInt(1 bits)
    val read = out Vec(UInt(bit_width bits), sub_cell_count)
    val store = in Vec(UInt(bit_width bits), sub_cell_count)
  }
  val address_write = UInt(log2Up(row_count) bits)
  address_write := io.address_write(log2Up(row_count)-1 downto 0)

  val address_read = UInt(log2Up(row_count) bits)
  address_read := io.address_read(log2Up(row_count)-1 downto 0)

  io.write_ena.map(_ := 0)
  io.read.map(_ := 0)

  for (i <- io.features_to_store) {
    i := io.store
  }

  when(address_write <= row_count){
    when(io.write_ena_user === 1) {
      io.write_ena(address_write) := 1
    }
  }
  when(address_write <= row_count){
    //when(io.write_ena_user === 0) {
      io.read := io.features_from_store(address_read)
    //}
  }
}

