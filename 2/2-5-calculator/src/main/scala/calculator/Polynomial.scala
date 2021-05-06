package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var( (b() * b() ) - ( 4 * a() * c() ) )
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var(delta() match {
      case x if x < 0 => Set()
      case _ => {
        val x1 = ( -b() + math.sqrt( delta() ) ) / ( 2 * a() * c() )
        val x2 = ( -b() - math.sqrt( delta() ) ) / ( 2 * a() * c() )
        Set(x1, x2)
      }
    })
  }
}
