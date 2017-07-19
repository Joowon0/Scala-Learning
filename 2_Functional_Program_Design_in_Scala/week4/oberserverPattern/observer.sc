import week4.oberserverPattern._

// the relationship is contained
// even if value of accts change
def consolidated(accts: List[BankAccount]): Signal[Int] =
  Signal(accts.map(_.balance()).sum)

val a = new BankAccount
val b = new BankAccount
val c = new Consolidator(List(a,b))
val d = consolidated(List(a, b))

c.totalBalance
d()

a deposit 30
b deposit 10

a.balance()

c.totalBalance
d()


val xchange = Signal(123)
val inDollar = Signal(d() * xchange())
inDollar()
b withdraw 5
inDollar()