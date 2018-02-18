package wk4

trait List[+T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
    def prepend[U >: T](elem:U): List[U] = new Cons(elem, this)
}

object Nil extends List[Nothing] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException("Nil.head")
    def tail: Nothing = throw new NoSuchElementException("Nil.tail")


    override def toString = ";"
}
class Cons[T](val head: T, val tail: List[T]) extends List[T]{
    def isEmpty = false
}


object test {
    val x: List[String] = Nil
}


object List {

def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, Nil ) )
def apply[T](x1: T): List[T] = new Cons( x1, Nil )
def apply[T]() = Nil

}
