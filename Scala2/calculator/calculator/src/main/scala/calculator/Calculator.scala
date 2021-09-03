package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
      namedExpressions.map((name, expr) => (name, Signal.Var(eval(expr(), namedExpressions))))

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double = {
    def evalWithUsedRefs(ex: Expr,used: Set[String]): Double = {
      ex match {
        case Literal(v) => v
        case Ref(name) => {
          val newExpr = getReferenceExpr(name, references)
          if (used.contains(name)) Double.NaN
          else evalWithUsedRefs(newExpr, used ++ Set(name))
        }
        case Plus(a, b) => evalWithUsedRefs(a,used) + evalWithUsedRefs(b,used)
        case Minus(a, b) => evalWithUsedRefs(a,used) - evalWithUsedRefs(b,used)
        case Times(a, b) => evalWithUsedRefs(a,used) * evalWithUsedRefs(b,used)
        case Divide(a, b) => evalWithUsedRefs(a,used) / evalWithUsedRefs(b,used)
      }
    }
    evalWithUsedRefs(expr, Set())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
