package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("demux0-1 example") {
    val in, out = new Wire
    demux(in, List(), List(out))
    in.setSignal(false)
    run

    assert(out.getSignal === false, "demux 1")

    in.setSignal(true)
    run

    assert(out.getSignal === true, "demux 2")
  }

  test("demux1-2 example") {
    val in, c0, out0, out1 = new Wire
    demux(in, List(c0), List(out1, out0))
    in.setSignal(false)
    c0.setSignal(false)
    run

    assert(out1.getSignal === false, "demux 1")
    assert(out0.getSignal === false, "demux 2")

    in.setSignal(false)
    c0.setSignal(true)
    run

    assert(out1.getSignal === false, "demux 3")
    assert(out0.getSignal === false, "demux 4")

    in.setSignal(true)
    c0.setSignal(false)
    run

    assert(out1.getSignal === false, "demux 5")
    assert(out0.getSignal === true, "demux 6")

    in.setSignal(true)
    c0.setSignal(true)
    run

    assert(out1.getSignal === true, "demux 7")
    assert(out0.getSignal === false, "demux 8")
  }

  test("demux2-4 example") {
    val in, c0, c1, out0, out1, out2, out3 = new Wire
    demux(in, List(c1, c0), List(out3, out2, out1, out0))
    in.setSignal(false)
    c1.setSignal(false)
    c0.setSignal(false)
    run

    assert(out3.getSignal === false, "demux 1")
    assert(out2.getSignal === false, "demux 2")
    assert(out1.getSignal === false, "demux 3")
    assert(out0.getSignal === false, "demux 4")

    c0.setSignal(true)
    run

    assert(out3.getSignal === false, "demux 5")
    assert(out2.getSignal === false, "demux 6")
    assert(out1.getSignal === false, "demux 7")
    assert(out0.getSignal === false, "demux 8")

    c1.setSignal(true)
    c0.setSignal(false)
    run

    assert(out3.getSignal === false, "demux 9")
    assert(out2.getSignal === false, "demux 10")
    assert(out1.getSignal === false, "demux 11")
    assert(out0.getSignal === false, "demux 12")

    c0.setSignal(true)
    run

    assert(out3.getSignal === false, "demux 13")
    assert(out2.getSignal === false, "demux 14")
    assert(out1.getSignal === false, "demux 15")
    assert(out0.getSignal === false, "demux 16")

    in.setSignal(true)
    c1.setSignal(false)
    c0.setSignal(false)
    run

    assert(out3.getSignal === false, "demux 17")
    assert(out2.getSignal === false, "demux 18")
    assert(out1.getSignal === false, "demux 19")
    assert(out0.getSignal === true, "demux 20")

    c0.setSignal(true)
    run

    assert(out3.getSignal === false, "demux 21")
    assert(out2.getSignal === false, "demux 22")
    assert(out1.getSignal === true, "demux 23")
    assert(out0.getSignal === false, "demux 24")

    c1.setSignal(true)
    c0.setSignal(false)
    run

    assert(out3.getSignal === false, "demux 25")
    assert(out2.getSignal === true, "demux 26")
    assert(out1.getSignal === false, "demux 27")
    assert(out0.getSignal === false, "demux 28")

    c0.setSignal(true)
    run

    assert(out3.getSignal === true, "demux 29")
    assert(out2.getSignal === false, "demux 30")
    assert(out1.getSignal === false, "demux 31")
    assert(out0.getSignal === false, "demux 32")
  }

  test("demux3-8 example") {
    val in, c1, c2, c3, out1, out2, out3, out4, out5, out6, out7, out8 = new Wire
    demux(
      in,
      List(c3, c2, c1),
      List(out8, out7, out6, out5, out4, out3, out2, out1))
    in.setSignal(false)
    c1.setSignal(false)
    c2.setSignal(false)
    c3.setSignal(false)
    run

    assert(out1.getSignal === false, "demux 1")
    assert(out2.getSignal === false, "demux 2")
    assert(out3.getSignal === false, "demux 3")
    assert(out4.getSignal === false, "demux 4")
    assert(out5.getSignal === false, "demux 5")
    assert(out6.getSignal === false, "demux 6")
    assert(out7.getSignal === false, "demux 7")
    assert(out8.getSignal === false, "demux 8")

    in.setSignal(true)
    run

    assert(out1.getSignal === true, "demux 9")
    assert(out2.getSignal === false, "demux 10")
    assert(out3.getSignal === false, "demux 11")
    assert(out4.getSignal === false, "demux 12")
    assert(out5.getSignal === false, "demux 13")
    assert(out6.getSignal === false, "demux 14")
    assert(out7.getSignal === false, "demux 15")
    assert(out8.getSignal === false, "demux 16")

    c1.setSignal(true)
    run

    assert(out1.getSignal === false, "demux 17")
    assert(out2.getSignal === true, "demux 18")
    assert(out3.getSignal === false, "demux 18")
    assert(out4.getSignal === false, "demux 20")
    assert(out5.getSignal === false, "demux 21")
    assert(out6.getSignal === false, "demux 22")
    assert(out7.getSignal === false, "demux 23")
    assert(out8.getSignal === false, "demux 24")

    c2.setSignal(true)
    run

    assert(out1.getSignal === false, "demux 25")
    assert(out2.getSignal === false, "demux 26")
    assert(out3.getSignal === false, "demux 27")
    assert(out4.getSignal === true, "demux 28")
    assert(out5.getSignal === false, "demux 29")
    assert(out6.getSignal === false, "demux 30")
    assert(out7.getSignal === false, "demux 31")
    assert(out8.getSignal === false, "demux 32")

    c3.setSignal(true)
    run

    assert(out1.getSignal === false, "demux 33")
    assert(out2.getSignal === false, "demux 34")
    assert(out3.getSignal === false, "demux 35")
    assert(out4.getSignal === false, "demux 36")
    assert(out5.getSignal === false, "demux 37")
    assert(out6.getSignal === false, "demux 38")
    assert(out7.getSignal === false, "demux 39")
    assert(out8.getSignal === true, "demux 40")
  }

}
