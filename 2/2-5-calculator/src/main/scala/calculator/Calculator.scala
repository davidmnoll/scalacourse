package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map( x => { (x._1 ,  Var( eval( x._2(), namedExpressions ) ) ) }  )
  }




  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case x : Literal => x.v

    case x : Ref => { if (references.contains(x.name)) eval( references(x.name)() , references.removed(x.name) ) else Double.NaN }

    case x : Divide => {
      val divisor = eval ( x.b, references )
      val numerator = eval (x.a, references )
      divisor match {
        case y if y != 0 => eval( x.a, references ) / divisor
        case y if y == 0 => Double.NaN
        case y if y == Double.NaN => Double.NaN
      }
    }
    case x : Times => eval( x.a, references ) * eval ( x.b, references )
    case x : Minus => eval( x.a, references ) - eval ( x.b, references )
    case x : Plus => eval( x.a, references ) + eval ( x.b, references )
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
