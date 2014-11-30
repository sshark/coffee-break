/* this solution causes StackOVerflowException because of the run method within the class
sealed trait Trampoline[+A] {
  def run: A = this match {
    case Done(a) => a
    case More(t) => t().run
  }
}
case class Done[A](a: A) extends Trampoline[A]
case class More[A](a: () => Trampoline[A]) extends Trampoline[A]

def odd(n : Int) : Trampoline[Boolean] = if (n == 0) Done(false) else More(() => even(n - 1))

def even(n: Int) : Trampoline[Boolean] = if (n == 0) Done(true) else More(() => odd(n - 1))
*/

// odd(9999).run


// custom solution
sealed trait Bounce[A]
case class Done[A](result: A) extends Bounce[A]
case class Call[A](thunk: () => Bounce[A]) extends Bounce[A]

def trampoline[A](bounce: Bounce[A]): A = bounce match {
  case Call(thunk) => trampoline(thunk())
  case Done(x) => x
}

def even2(n: Int): Bounce[Boolean] = {
  if (n == 0) Done(true)
  else Call(() => odd2(n - 1))
}

def odd2(n: Int): Bounce[Boolean] = {
  if (n == 0) Done(false)
  else Call(() => even2(n - 1))
}

trampoline(even2(9999))

// using Scala library
import scala.util.control.TailCalls._

def even3(n:Int) : TailRec[Boolean] = if (n==0) done(true) else tailcall(odd3(n - 1))
def odd3(n:Int) : TailRec[Boolean]= if (n==0) done(false) else tailcall(even3(n-1))

even3(9999).result
def isEven(xs: List[Int]): TailRec[Boolean] =
  if (xs.isEmpty) done(true) else tailcall(isOdd(xs.tail))
def isOdd(xs: List[Int]): TailRec[Boolean] =
  if (xs.isEmpty) done(false) else tailcall(isEven(xs.tail))
isEven((1 to 100000).toList).result

