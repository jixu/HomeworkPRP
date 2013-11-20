package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val a3, a4, a5 = new Wire
    inverter(a1, a3)
    inverter(a2, a4)
    andGate(a3, a4, a5)
    inverter(a5, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    c match {
      case List() => in addAction { () => afterDelay(0) {out(0).setSignal(in.getSignal)} }
      case cn_1 :: tail =>
        val d0, d1 = new Wire
        val t = new Wire
        andGate(in, cn_1, d1)
        inverter(cn_1, t)
        andGate(in, t, d0)
        demux(d1, tail, out.take(out.size/2))
        demux(d0, tail, out.drop(out.size/2))
    }
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //

  def orGateExample {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in11", in1)
    probe("in21", in2)
    probe("out1", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def demuxGateExample {
    val in = new Wire
    val c = List.fill(2)(new Wire)
    val out = List.fill(4)(new Wire)

    demux(in, c, out)

    probe("in", in)
    c zip (0 until c.size) foreach { p =>
      probe("c" + p._2, p._1)
      p._1.setSignal(false)
    }
    out zip (0 until out.size) foreach { p =>
      probe("out" + p._2, p._1)
    }
    run

    in.setSignal(true)
    run

    c(0).setSignal(true)
    run

    c(1).setSignal(true)
    run

    c(0).setSignal(false)
    run

  }
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  // Circuit.andGateExample
  // Circuit.orGateExample
  Circuit.demuxGateExample
}
