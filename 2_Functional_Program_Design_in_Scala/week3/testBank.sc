import week3._

object testBank {
  val account = new bankWithState
  val de1 = account.deposit(100)
  val de2 = account.withdraw(50)
}