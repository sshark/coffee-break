/**
 *
 * @author Lim, Teck Hooi
 *
 *
 */

class VendingMachine {
}

trait VendingMachineState {
  def insertCoins(coins : Int)
  def pressButton()
  def chooseDrink(choice : Int)
  def reset()
}

class IdleState extends VendingMachineState {
  override def insertCoins(coins: Int): Unit = ???

  override def chooseDrink(choice: Int): Unit = ???

  override def reset(): Unit = ???

  override def pressButton(): Unit = ???
}

class CoinsInsertedState extends VendingMachineState {
  override def insertCoins(coins: Int): Unit = ???

  override def chooseDrink(choice: Int): Unit = ???

  override def reset(): Unit = ???

  override def pressButton(): Unit = ???
}

class ReadyState extends VendingMachineState {
  override def insertCoins(coins: Int): Unit = ???

  override def chooseDrink(choice: Int): Unit = ???

  override def reset(): Unit = ???

  override def pressButton(): Unit = ???
}

class DispensedState extends VendingMachineState {
  override def insertCoins(coins: Int): Unit = ???

  override def chooseDrink(choice: Int): Unit = ???

  override def reset(): Unit = ???

  override def pressButton(): Unit = ???
}

class MachineException(message : String, cause : Throwable) extends Exception(message, cause) {
  def this(message : String) = {
    this(message, null)
  }
}
