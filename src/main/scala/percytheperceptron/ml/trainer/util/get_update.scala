package percytheperceptron.ml.trainer.util

import spinal.core._

class get_update(bit_width: Int) extends Component {
  val io = new Bundle {
    val eta, actual, predicted = in SInt (bit_width bits)
    val y = out SInt (bit_width bits)
  }
  val error: SInt = SInt(bit_width bits)
  error := io.actual - io.predicted

  val update_pre: SInt = SInt(2*bit_width bits)
  val update: SInt = SInt(bit_width bits)
  update_pre := io.eta * error
  update := update_pre(bit_width-1 downto 0)

  io.y := update
}

