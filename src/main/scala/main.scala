import scala.annotation.tailrec
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
  println("\nZadanie 4:")
  println(bell.take(10).toList)
  println(stream_head(bell))
  println(stream_tail(bell).take(10).toList)
  println(getStreamElements(bell, 10))
  println(getEverySecondStreamElement(bell, 5, true))
  println(getEverySecondStreamElement(bell, 5, false))
  println(getAfterSkipStreamElements(bell, 5, 3))
  println(getSumStreamElements(bell, natural, 10))
  println(getFunctionStream(natural, (x: Int) => x +1).take(10).toList)
}


var stirling_memory: mutable.HashMap[(Int,Int),Int] = new mutable.HashMap()

// Zadanie 1:
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

//Zadanie 2:
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

//Zadanie 4:
lazy val bell: Stream[Int] = {
  def loop(n: Int): Stream[Int] = {
    val stirlingNumbers = (0 to n).map(m => stirling(n, m))
    stirlingNumbers.sum #:: loop(n + 1)
  }
  loop(0)
}

//b:
def stream_head[A](stream: Stream[A]): Option[A] = {
  stream match {
    case h #:: _ => Some(h)
    case _ => None
  }
}

def stream_tail[A](stream: Stream[A]): Stream[A] = {
  stream match {
    case _ #:: tail => tail
    case _ => Stream.empty
  }
}

//c: i)
def getStreamElements[A](stream: Stream[A], n: Int): List[A] = {
  if (n <= 0) {
    return List()
  } else {
    stream match {
      case h #:: tail => h :: getStreamElements(tail, n - 1)
      case _ => List()
    }
  }
}

//c: ii)
def getEverySecondStreamElement[A](stream: Stream[A], n: Int, startWithFirst: Boolean): List[A] = {
  if (n <= 0) {
    return List()
  }
  if (startWithFirst) {
    stream match {
    case h #:: tail => h :: getEverySecondStreamElement(tail, n - 1, false)
    case _ => List ()
    }
  } else {
    getEverySecondStreamElement(stream.tail, n, true)
  }
}

//c: iii)
@tailrec
def getAfterSkipStreamElements[A](Stream: Stream[A], n: Int, skip: Int): List[A] = {
  if (n <= 0) {
    return List()
  }
  if (skip <= 0) {
    getStreamElements(Stream, n)
  } else {
    getAfterSkipStreamElements(Stream.tail, n, skip - 1)
  }
}

//c: iv)
lazy val natural: Stream[Int] = {
  def loop(n: Int): Stream[Int] = {
    n #:: loop(n + 1)
  }
  loop(0)
}

def getSumStreamElements(stream1: Stream[Int], stream2: Stream[Int], n: Int): List[Int] = {
  if (n <= 0) {
    return List()
  } else {
    (stream1, stream2) match {
      case (h1 #:: tail1, h2 #:: tail2) => (h1+h2) :: getSumStreamElements(tail1, tail2, n - 1)
      case _ => List()
    }
  }
}

//c: v)
def getFunctionStream[A,B](stream: Stream[A], f: (A => B)): Stream[B] = {
  stream match {
    case h #:: tail => f(h) #:: getFunctionStream(tail, f)
    case _ => Stream.empty
  }
}
