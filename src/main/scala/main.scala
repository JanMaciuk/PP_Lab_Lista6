import scala.collection.mutable
import scala.collection.mutable.HashMap

@main
def main(): Unit = {
  println("Hello world!")
  println("Zadanie 1:")
  println(stirling(5,2))
  println(memorized_stirling(99,97))
  println(memorized_stirling(99,97))
  println("\nZadanie 2:")
  val made_memorized_stirling = make_memorize(stirling)
  println(made_memorized_stirling(99,97))
  println(made_memorized_stirling(99,97))
  println("\nZadanie 3:")
  lazy val number =stirlingAndPrint(99,97)
  println("This prints before the function is called, because its lazy")
  println(number) // stirlingAndPrint will only be called now
}

var stirling_memory: mutable.HashMap[(Int,Int),Int] = new mutable.HashMap()


def stirling (n:Int,m:Int):Int = {
  (n,m) match
    case (n,0) => 0
    case (n,m) if n==m => 1
    case (n,m) if n<m => 0
    case (n,m) => stirling(n-1,m-1) + m*stirling(n-1,m)
}
def stirlingAndPrint(n:Int,m:Int):Int = {
  println("Stirling function is called")
  return stirling(n,m)

}


def memorized_stirling(n:Int,m:Int):Int = {
    if (stirling_memory.contains((n,m))) {
        return stirling_memory((n,m))
    }
    else {
        val result = stirling(n,m)
        stirling_memory += ((n,m) -> result)
        return result
    }
}

def make_memorize[A, B](fun: (A, A) => B): (A, A) => B = {
  val memory: mutable.HashMap[(A, A), B] = new mutable.HashMap()

  // Define the memorized function with two arguments
  def memorized_fun(arg1: A, arg2: A): B = {
    val key = (arg1, arg2)

    if (memory.contains(key)) {
      return memory(key)
    } else {
      val result = fun(arg1, arg2)
      memory += (key -> result)
      return result
    }
  }

  return memorized_fun
}