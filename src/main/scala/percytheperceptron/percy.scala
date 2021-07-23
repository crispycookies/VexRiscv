package percytheperceptron

import percytheperceptron.memory.{register_file, shift_register}
import percytheperceptron.ml.perceptron.{perceptron, perceptron_with_reg}
import percytheperceptron.ml.perceptron.util.{activation, perceptron_core}
import percytheperceptron.ml.trainer.{jimenez_trainer, trainer}
import percytheperceptron.ml.trainer.util.get_update
import spinal.core._

// This is the main function that generates the VHDL and the Verilog corresponding to MyTopLevel.
object PercyMain {
  def main(args: Array[String]) {
    //SpinalVerilog(new trainer_with_percy(bit_width = 16, feature_count = 4, lower_bound = -1, upper_bound = 1, zero = 0))
    SpinalVhdl(new perceptron(bit_width = 16, feature_count = 4, lower_bound = -1, upper_bound = 1, zero = 0))
    SpinalVhdl(new mod_index(address_bit_width = 16, index_bit_width = 4, 4))
    SpinalVhdl(new shift_register( 4, 1))
    SpinalVhdl(new jimenez_trainer( 8, 2, 127))
    SpinalVhdl(new predictor_jimenez( 16, 4, 4, 4, 4, -1, 1, 0, 3, 128))
  }
}
