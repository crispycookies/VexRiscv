package percytheperceptron

import spinal.core._
import spinal.lib._

import percytheperceptron.memory.shift_register

class mod_read_write_index(address_bit_width : Int, index_bit_width : Int, table_size : Int, delay : Int) extends Component {
  val io = new Bundle {
    val address = in UInt(address_bit_width bits)
    val index_read = out UInt (index_bit_width bits)
    val index_write = out UInt (index_bit_width bits)
  }
  val mod_index_instance = new mod_index(address_bit_width, index_bit_width, table_size)

  val index_buffer = new shift_register(bit_width = index_bit_width, depth = delay)
  index_buffer.io.input := mod_index_instance.io.index.asSInt
  mod_index_instance.io.address := io.address

  io.index_read := mod_index_instance.io.index
  io.index_write := index_buffer.io.output.asUInt
}

