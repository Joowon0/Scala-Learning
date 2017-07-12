package week3.digitalCircuitSimulator

/**
  * Created by joowon on 17. 7. 12.
  */
class Simulation {
  type Action = () => Unit

  case class Event(time: Int, action: Action)

  // The current simulated time
  private var curtime = 0
  def currentTime: Int = curtime

  private type Agenda = List[Event]
  private var agenda: Agenda = List() // time sorted

  private def insert(ag: List[Event], item: Event): List[Event] =
    ag match {
      case first :: rest if first.time <= item.time =>
        first :: insert(rest, item)
      case _ => item :: ag
    }

  // Registers an actions 'block' to perform after a given delay
  // relative to the current time
  def afterDelay(delay: Int)(block: => Unit) = {
    val item = Event(curtime + delay, () => block)
    agenda = insert(agenda, item)
  }

  // Performs the simulation until there are no actions waiting
  def run(): Unit = {
    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + " ***")
    }
    loop()
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case Nil =>
  }



}
