package week4.oberserverPattern

/**
  * Created by joowon on 17. 7. 19.
  * contain total amount of observed list
  */
class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.subscribe(this))

  private var total: Int = _
  def totalBalance = total

  private def compute() =
    total = observed.map(_.balance()).sum

  override def handler(pub: Publisher): Unit = compute()
}
