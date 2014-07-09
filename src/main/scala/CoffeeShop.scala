import java.util.concurrent.Executors

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

/**
 *
 * @author Lim, Teck Hooi
 *
 *
 */

sealed class Drink(val name : String, val price : Double)
case class Expresso() extends Drink("Expresso", 10d)
case class Latte() extends Drink("Latte", 15d)
case class OrangeJuice() extends Drink("Orange juice", 6d)

case class Order(drink : Drink, customerName : String)
case class Payment(amount : Double, customer : String)
case class Receipt(amount : Double)

case object CloseShop
case object OrderDrink
case object Revenue


object Menu {
  val drinks = List(Expresso, Latte, OrangeJuice)

  def recommendADrink = {
    drinks(Random.nextInt(drinks.size))
  }
}

class Barrister extends Actor {
  def receive = {
    case order : Order =>
      val drink = order.drink
      println("Preparing " + drink.name)
      Thread.sleep(Random.nextInt(2000))
      println(drink.name + " ready for " + order.customerName)
  }
}

class Customer(name : String, cashier : ActorRef) extends Actor {

  def receive = {
    case receipt : Receipt =>
      Thread.sleep(Random.nextInt(2000))
      sender ! Payment(receipt.amount, name)

    case OrderDrink => cashier ! Order(Menu.recommendADrink(), name)

    case drink : Drink => println(name + " is happily drink his " + drink.name)
  }
}

class Cashier extends Actor {
  var total : Double = 0d
  val barrister = context.system.actorOf(Props[Barrister])

  def receive = {
    case order : Order =>
      val drink = order.drink
      println("Asking money for $" + drink.price + " for " + drink.name + " from " + order.customerName)
      Thread.sleep(Random.nextInt(1000))
      total += drink.price

      implicit val timeout = Timeout(1 seconds)
      import context.dispatcher
      val customer = sender()
      val fixDrink = barrister ? drink
      fixDrink.onComplete(drinkReady => customer ! drinkReady)

      sender ! Receipt(drink.price)

    case payment : Payment =>
      println("Registering payment of $" + payment.amount)
      Thread.sleep(Random.nextInt(1500))
      println("Thank you, " + payment.customer)

    case Revenue => sender ! total

    case CloseShop =>
      println("Closing coffee shop/")
/*
      self ! PoisonPill
      barrister ! PoisonPill
      sender ! total
*/
  }
}

object CoffeeShop extends App {
  override
  def main(args: Array[String]) {

    val pool = Executors.newCachedThreadPool()
    implicit val ec = ExecutionContext.fromExecutorService(pool)

    val coffeeShop = ActorSystem("FamousCoffeeShop")
    val cashier = coffeeShop.actorOf(Props[Cashier])
    implicit val timeout = Timeout(5 seconds)

    val customers = (1 to 4).map{case i =>
      val customer = coffeeShop.actorOf(Props(classOf[Customer], i.toString, cashier))
      customer ? OrderDrink
    }

    Await.ready(Future.sequence(customers), 10 seconds)

    val revenue = cashier ? Revenue // TODO how to provide type to Future
    revenue.onComplete(r => println("Cashier has collected $" + r.get))
    Await.ready(revenue, 10 seconds)

    cashier ! CloseShop
    coffeeShop.shutdown()
    pool.shutdown()
    println("Coffee shop closed.")
  }
}
