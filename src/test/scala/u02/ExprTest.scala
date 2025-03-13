package u02

import org.junit.*
import org.junit.Assert.*
import Lab2.*

class ExprTest:

  val expr1 = Expr.Literal(5) //5
  val expr2 = Expr.Add(Expr.Literal(2), Expr.Literal(3)) // 2 + 3
  val expr3 = Expr.Multiply(Expr.Literal(4), expr2) // 4 * (2 + 3)

  @Test def testEvaluateLiteral() =
    assertEquals(5, evaluate(expr1))

  @Test def testEvaluateAdd() =
    assertEquals(5, evaluate(expr2))

  @Test def testEvaluateMultiply() =
    assertEquals(20, evaluate(expr3))

  @Test def testShowLiteral() =
    assertEquals("5", show(expr1))

  @Test def testShowAdd() =
    assertEquals("(2 + 3)", show(expr2))

  @Test def testShowMultiply() =
    assertEquals("(4 * (2 + 3))", show(expr3))



