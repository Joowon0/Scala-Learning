package week4.oberserverPattern

/**
  * Created by joowon on 17. 7. 19.
  */
class BankAccount extends Publisher{
  val balance = Var(0)

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      val b = balance()
      balance() = b + amount
      publish()
    }

  def withdraw(amount: Int): Unit =
    if (0 < amount && amount <= balance()) {
      val b = balance()
      balance() = b - amount
      publish()
    }
    else throw new Error("insufficient funds")
}
