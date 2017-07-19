package week3.digitalCircuitSimulator

/**
  * Created by joowon on 17. 7. 12.
  */
abstract class Gates extends Simulation {

  def InverterDelay: Int
  def AndGateDelay : Int
  def OrGateDelay  : Int

  class Wire {
    // current value of signal
    private var sigVal = false
    //actions currently attached to the wire
    private var actions: List[Action] = Nil

    def getSignal(): Boolean = sigVal
    def setSignal(sig: Boolean): Unit = {
      if (sig != sigVal) {
        sigVal = sig
        actions foreach (_())
      }
    }
    def addAction(a: Action): Unit = {
      actions = a :: actions
      a() // why??
    }
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      // get signal should happen b4 InverterDelay
      val inputSig = input.getSignal()
      afterDelay(InverterDelay) {output setSignal !inputSig}
    }
    input addAction invertAction
  }
  def andGate (in1: Wire, in2: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      // get signal should happen b4 AndGateDelay
      val in1Sig = in1.getSignal()
      val in2Sig = in2.getSignal()
      afterDelay(AndGateDelay) { output setSignal (in1Sig & in2Sig)}
    }
    in1 addAction andAction
    in2 addAction andAction
  }
  def orGate (in1: Wire, in2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      // get signal should happen b4 OrGateDelay
      val in1Sig = in1.getSignal()
      val in2Sig = in2.getSignal()
      afterDelay(OrGateDelay) { output setSignal (in1Sig | in2Sig)}
    }
    in1 addAction orAction
    in2 addAction orAction
  }
  // a | b = !( !a & !b )
  def orGateAlt(in1: Wire, in2: Wire, output: Wire): Unit = {
    val notIn1, notIn2, notOut = new Wire
    inverter(in1, notIn1)
    inverter(in2, notIn2)
    andGate(notIn1, notIn2, notOut)
    inverter(notOut, output)
  }

  // examine the changes of the signals on a specific wire
  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime value = ${wire.getSignal}")
    }
    wire addAction probeAction
  }

}
