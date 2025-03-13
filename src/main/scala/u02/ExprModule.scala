package u02

object ExprModule:

  println("Task 9")
  println("Sum types, product types, modules")
  println()

  enum Expr:
    case Literal (const: Int)
    case Add (exp1: Expr, exp2: Expr)
    case Multiply (exp1: Expr, exp2: Expr)

  def evaluate(expr: Expr): Int = expr match
    case Expr.Literal(const) => const
    case Expr.Add(exp1, exp2) => evaluate(exp1) + evaluate(exp2)
    case Expr.Multiply(exp1, exp2) => evaluate(exp1) * evaluate(exp2)

  def show(expr: Expr): String = expr match
    case Expr.Literal(const) => const.toString
    case Expr.Add(exp1, exp2) => s"(${show(exp1)} + ${show(exp2)})"
    case Expr.Multiply(exp1, exp2) => s"(${show(exp1)} * ${show(exp2)})"

