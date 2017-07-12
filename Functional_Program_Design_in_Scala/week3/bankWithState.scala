package week3

/**
  * Created by joowon on 17. 7. 10.
  * this is for lecture 3.1
  */
class bankWithState {
  private var balance = 0
  def deposit(amount: Int):Int = {
    if (amount > 0)
      balance = balance + amount
    balance
  }

  def withdraw(amount: Int):Int = {
    if (amount > 0 && balance >= amount)
      balance = balance - amount
    balance
  }
}
