package funsets

object Main extends App {
  import FunSets._
  // println( exercise.product(x => x)(3,4) )
  // println( exercise.fact(4) )

  println(contains(singletonSet(1), 1))
}

object Exercise{
    def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce( f, (x,y) => x * y, 1 )(a, b)
    def fact(n: Int) = product(x => x)(1, n)

    def mapReduce(f: Int=> Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
        if (a > b) zero
        else combine( f(a), mapReduce(f, combine, zero)( a+1, b ) )
    }

}
