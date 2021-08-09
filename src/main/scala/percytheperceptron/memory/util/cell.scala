package percytheperceptron.memory.util

import spinal.core._

class cell(bit_width : Int, sub_cell_count : Int) extends Component {
  val io = new Bundle {
    val write_ena = in UInt (1 bits)
    val features_to_store = in Vec(UInt(bit_width bits), sub_cell_count)
    val features_from_store = out Vec(UInt(bit_width bits), sub_cell_count)
  }
  val cntReg = Vec(Reg(UInt(bit_width bits)) init(1), sub_cell_count)
  cntReg:=Mux(io.write_ena === 1, io.features_to_store, cntReg)
  io.features_from_store := cntReg
}

