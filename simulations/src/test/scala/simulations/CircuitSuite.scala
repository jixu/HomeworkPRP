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

  test("demux example") {
    val in = new Wire
    val c = List.fill(2)(new Wire)
    val out = List.fill(4)(new Wire)

    demux(in, c, out)

    c zip (0 until c.size) foreach { p =>
      p._1.setSignal(false)
    }
    out zip (0 until out.size) foreach { p =>
      p._1.setSignal(false)
    }
    run
    assert(out.map(_.getSignal) === List(false, false, false, false), "demux 1")

    in.setSignal(true)
    run
    assert(out.map(_.getSignal) === List(false, false, false, true), "demux 2")

    c(0).setSignal(true)
    run
    assert(out.map(_.getSignal) === List(false, true, false, false), "demux 3")

    c(1).setSignal(true)
    run
    assert(out.map(_.getSignal) === List(true, false, false, false), "demux 4")

    c(0).setSignal(false)
    run
    assert(out.map(_.getSignal) === List(false, false, true, false), "demux 5")
  }
}
