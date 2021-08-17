package vexriscv

import java.io.File

import org.scalatest.FunSuite
import spinal.core.SpinalVerilog
import vexriscv.demo._

import scala.sys.process._

class DhrystoneBench extends FunSuite {
  var it = 0
  def doCmd(cmd: String): String = {
    val stdOut = new StringBuilder()
    class Logger extends ProcessLogger {
      override def err(s: => String): Unit = {
        if (!s.startsWith("ar: creating ")) println(s)
      }

      override def out(s: => String): Unit = {
        println(s)
        stdOut ++= s
      }

      override def buffer[T](f: => T) = f
    }
    Process(cmd, new File("src/test/cpp/regression")).!(new Logger)
    stdOut.toString()
  }

  val report = new StringBuilder()

  def getDmips(name: String, gen: => Unit, testCmd: String): Unit = {
    var genPassed = false
    it += 1
    test(name + "_gen" + it) {
      gen
      genPassed = true
    }
    test(name + "_test" + it) {
      assert(genPassed)
      val str = doCmd(testCmd)
      assert(!str.contains("FAIL"))
      val intFind = "(\\d+\\.?)+".r
      val dmips = intFind.findFirstIn("DMIPS per Mhz\\:                              (\\d+.?)+".r.findAllIn(str).toList.last).get.toDouble
      val coremarkTicks = intFind.findFirstIn("Total ticks      \\: (\\d+.?)+".r.findAllIn(str).toList.last).get.toDouble
      val coremarkIterations = intFind.findFirstIn("Iterations       \\: (\\d+.?)+".r.findAllIn(str).toList.last).get.toDouble
      val coremarkHzs = intFind.findFirstIn("DCLOCKS_PER_SEC=(\\d+.?)+".r.findAllIn(str).toList.last).get.toDouble
      val coremarkPerMhz = 1e6 * coremarkIterations / coremarkTicks
      report ++= s"$name -> $dmips DMIPS/Mhz $coremarkPerMhz Coremark/Mhz\n"
    }

  }

  getDmips(
    name = "GenFull",
    gen = GenFull.main(null),
    testCmd = "make clean run REDO=10 CSR=no MMU=no  COREMARK=yes TRACE=no"
  )
  getDmips(
    name = "GenFull",
    gen = GenFull.main(null),
    testCmd = "make clean run REDO=10 CSR=no MMU=no  COREMARK=yes TRACE=no"
  )
  getDmips(
    name = "GenFull",
    gen = GenFull.main(null),
    testCmd = "make clean run REDO=10 CSR=no MMU=no  COREMARK=yes TRACE=no"
  )
  getDmips(
    name = "GenFull",
    gen = GenFull.main(null),
    testCmd = "make clean run REDO=10 CSR=no MMU=no  COREMARK=yes TRACE=no"
  )
  getDmips(
    name = "GenFull",
    gen = GenFull.main(null),
    testCmd = "make clean run REDO=10 CSR=no MMU=no  COREMARK=yes TRACE=no"
  )
  getDmips(
    name = "GenFull",
    gen = GenFull.main(null),
    testCmd = "make clean run REDO=10 CSR=no MMU=no  COREMARK=yes TRACE=no"
  )
  getDmips(
    name = "GenFull",
    gen = GenFull.main(null),
    testCmd = "make clean run REDO=10 CSR=no MMU=no  COREMARK=yes TRACE=no"
  )
  getDmips(
    name = "GenFull",
    gen = GenFull.main(null),
    testCmd = "make clean run REDO=10 CSR=no MMU=no  COREMARK=yes TRACE=no"
  )
  getDmips(
    name = "GenFull",
    gen = GenFull.main(null),
    testCmd = "make clean run REDO=10 CSR=no MMU=no  COREMARK=yes TRACE=no"
  )
  getDmips(
    name = "GenFull",
    gen = GenFull.main(null),
    testCmd = "make clean run REDO=10 CSR=no MMU=no  COREMARK=yes TRACE=no"
  )
  getDmips(
    name = "GenFull",
    gen = GenFull.main(null),
    testCmd = "make clean run REDO=10 CSR=no MMU=no  COREMARK=yes TRACE=no"
  )
  getDmips(
    name = "GenFull",
    gen = GenFull.main(null),
    testCmd = "make clean run REDO=10 CSR=no MMU=no  COREMARK=yes TRACE=no"
  )

  test("final_report") {
    println(report)
  }
}
