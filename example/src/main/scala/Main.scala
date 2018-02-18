import wk4.List._


object Main extends App {
    var test = List(2,3)

    def f(xs: List[NonEmpty], x: Empty) = xs prepend x

    f( test, new Empty )
}
