package percytheperceptron

import spinal.core._

class mod_index(address_bit_width : Int, index_bit_width : Int, table_size : Int) extends Component {
  val io = new Bundle {
    val address = in UInt(address_bit_width bits)
    val index = out UInt (index_bit_width bits)
  }
  val index_not_truncd = UInt(address_bit_width bits)
  index_not_truncd := io.address % table_size
  io.index := index_not_truncd(index_bit_width-1 downto 0)
}
