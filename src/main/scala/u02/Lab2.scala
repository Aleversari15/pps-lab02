package u02

object Lab2 extends App:
  //TASKS - FUNCTIONS
  println("Task 3")
  println("a) positive function")
  println()

  println("Literal function")

  val literalPositive: Int => String = _ match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println(s"Test literal positive function with input 10. Expected: positive, Result: ${literalPositive(10)}")
  println(s"Test literal positive function with input -10. Expected: negative, Result: ${literalPositive(-10)}")
  println()
  
  println("Method")

  def methodPositive(x: Int): String = x match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println(s"Test positive method with input 10. Expected: positive, Result: ${methodPositive(10)}")
  println(s"Test positive method with input -10. Expected: negative, Result: ${ methodPositive(-10)}")
  println()

  println("b) neg function")
  println()
  def neg(predicate: String => Boolean): String => Boolean = (s: String) => !predicate(s)

  val empty: String => Boolean = _ == ""
  val notEmpty = neg(empty)

  println(s"Test notEmpty function passing 'foo' as input. Expected: true, Result: ${notEmpty("foo")}")
  println(s"Test notEmpty function passing an empty string as input. Expected: false, Result: ${notEmpty("")}")
  println(s"Test notEmpty function passing '(notEmpty(\"foo\") && !notEmpty(\"\"))' as input. Expected: true, Result:${(notEmpty("foo") && !notEmpty(""))}")
  println()

  println("c) generic neg function")
  println()
  def genericNeg[X](predicate: X => Boolean): X => Boolean = (x: X) => !predicate(x)

  val isZero: Int => Boolean = _ == 0
  val notZero = genericNeg(isZero)
  val notEmptyWithGenerics = genericNeg(empty)

  // Test per la funzione notZero
  println(s"Test notZero function passing 2 as input. Expected: true, Result: ${notZero(2)}")
  println(s"Test notZero function passing 0 as input. Expected: false, Result: ${notZero(0)}")
  println(s"Test notZero function passing '(notZero(2) && !notZero(0))' as input. Expected: true, Result: ${(notZero(2) && !notZero(0))}")

  // Test per la funzione notEmptyWithGenerics
  println(s"Test notEmptyWithGenerics function passing 'foo' as input. Expected: true, Result: ${notEmptyWithGenerics("foo")}")
  println(s"Test notEmptyWithGenerics function passing an empty string as input. Expected: false, Result: ${notEmptyWithGenerics("")}")
  println(s"Test notEmptyWithGenerics function passing '(notEmptyWithGenerics(\"foo\") && !notEmptyWithGenerics(\"\"))' as input. Expected: true, Result: ${(notEmptyWithGenerics("foo") && !notEmptyWithGenerics(""))}")


