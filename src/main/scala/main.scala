import scala.collection.mutable
import scala.collection.mutable.HashMap

@main
def main(): Unit = {
  println("Hello world!")
  println("Zadanie 1:")
  println(stirling(5,2));
  println(memorized_stirling(999,998));
  println(memorized_stirling(999,998));
  //todo: call
}

var stirling_memory: mutable.HashMap[(Int,Int),Int] = new mutable.HashMap();


def stirling (n:Int,m:Int):Int = {
  (n,m) match
    case (n,0) => 0
    case (n,m) if n==m => 1
    case (n,m) if n<m => 0
    case (n,m) => stirling(n-1,m-1) + m*stirling(n-1,m)
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

def make_memorize[A,B](fun: A=>B):A=>B = {
    val memory: mutable.HashMap[A,B] = new mutable.HashMap();
    def memorized_fun(arg:A):B = {
        if (memory.contains(arg)) {
            return memory(arg)
        }
        else {
            val result = fun(arg)
            memory += ((arg) -> result)
            return result
        }
    }
    return memorized_fun
}