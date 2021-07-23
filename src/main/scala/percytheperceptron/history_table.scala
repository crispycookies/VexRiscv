package percytheperceptron

import spinal.core._
import spinal.lib._

class history_table(bit_width: Int, feature_count: Int) extends Component {
  val io = new Bundle {
    val taken = in UInt(1 bit)
    val history = out Vec(SInt(bit_width bits), feature_count)
  }
  val shift_reg = Reg(SInt(feature_count bits)) init(0)
  shift_reg := shift_reg |<<1

  when(io.taken === 1) {
    shift_reg.lsb := True
  } otherwise {
    shift_reg.lsb := False
  }

  for (i <- 0 until feature_count) {
    when(shift_reg(i)) {
      io.history(i) := 1
    } otherwise {
      io.history(i) := 0
    }
  }
}

