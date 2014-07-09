import akka.actor.{Actor, ActorSystem, Props}

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

case class Order(drink : Drink, customer : String)
case class Payment(amount : Double, customer : String)
case object CloseShop


object Menu {
  val drinks = List(Expresso, Latte, OrangeJuice)

  def recommendADrink = {
    drinks(Random.nextInt(drinks.size))
  }

  def placeAnOrderForCustomer(name : String) = {
    Order(recommendADrink(), name)
  }
}

class Barrister extends Actor {
  def receive = {
    case order : Order => {
      val drink = order.drink
      println("Preparing " + drink.name)
      Thread.sleep(Random.nextInt(2000))
      println(drink.name + " ready for " + order.customer)
    }
  }
}

class Cashier extends Actor {
  var total : Double = 0d

  def receive = {
    case order : Order => {
      val drink = order.drink
      println("Asking money for $" + drink.price + " for " + drink.name + " from " + order.customer)
      Thread.sleep(Random.nextInt(1000))
      total += drink.price
      sender ! drink.price
    }

    case payment : Payment => {
      println("Registering payment of $" + payment.amount)
      Thread.sleep(Random.nextInt(1500))
      println("Thank you, " + payment.customer)
    }

    case CloseShop => {
      println("Closing shop. Calculate total sales.")
      sender ! total
    }
  }
}

object CoffeeShop extends App {
  override
  def main(args: Array[String]) {
    val coffeeShop = ActorSystem("FamousCoffeeShop")

    val cashier = coffeeShop.actorOf(Props[Cashier])
    val barrister = coffeeShop.actorOf(Props[Barrister])


    (1 to 4).foreach{case i => {
      val order = Menu.placeAnOrderForCustomer(i.toString)
      barrister ! order
      cashier ! order
    }}
  }
}
