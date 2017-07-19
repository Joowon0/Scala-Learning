package week4.oberserverPattern

/**
  * Created by joowon on 17. 7. 19.
  */

trait Subscriber {
  def handler(pub: Publisher)
}