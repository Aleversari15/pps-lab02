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

  println(s"Test notZero function passing 2 as input. Expected: true, Result: ${notZero(2)}")
  println(s"Test notZero function passing 0 as input. Expected: false, Result: ${notZero(0)}")
  println(s"Test notZero function passing '(notZero(2) && !notZero(0))' as input. Expected: true, Result: ${(notZero(2) && !notZero(0))}")

  println(s"Test notEmptyWithGenerics function passing 'foo' as input. Expected: true, Result: ${notEmptyWithGenerics("foo")}")
  println(s"Test notEmptyWithGenerics function passing an empty string as input. Expected: false, Result: ${notEmptyWithGenerics("")}")
  println(s"Test notEmptyWithGenerics function passing '(notEmptyWithGenerics(\"foo\") && !notEmptyWithGenerics(\"\"))' as input. Expected: true, Result: ${(notEmptyWithGenerics("foo") && !notEmptyWithGenerics(""))}")
  println()

  println("Task 4")
  println("Currying")
  println()

  val p1: Int => Int => Int => Boolean = x => y => z => x<=y && y==z
  val p2: (Int, Int, Int) => Boolean = (x,y,z) => x<=y && y==z
  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z
  def p4(x: Int, y: Int, z: Int): Boolean = x <= y && y == z

  println(s"Test p1 function passing (1, 2, 2) as input. Expected: true, Result: ${p1(1)(2)(2)}")
  println(s"Test p1 function passing (3, 2, 2) as input. Expected: false, Result: ${p1(3)(2)(2)}")

  println(s"Test p2 function passing (1, 2, 2) as input. Expected: true, Result: ${p2(1, 2, 2)}")
  println(s"Test p2 function passing (2, 2, 3) as input. Expected: false, Result: ${p2(2, 2, 3)}")

  println(s"Test p3 function passing (1, 2, 2) as input. Expected: true, Result: ${p3(1)(2)(2)}")
  println(s"Test p3 function passing (2, 1, 1) as input. Expected: false, Result: ${p3(2)(1)(1)}")

  println(s"Test p4 function passing (1, 2, 2) as input. Expected: true, Result: ${p4(1, 2, 2)}")
  println(s"Test p4 function passing (3, 2, 2) as input. Expected: false, Result: ${p4(3, 2, 2)}")
  println()

  println("Task 5")
  println("Functional compositions - 2 functions")
  println()
  //def compose(f: Int => Int, g: Int => Int): Int => Int
  //f(g(x))
  def composeGeneric[A,B,C](f: B => C, g: A => B): A => C = (x: A) => f(g(x))

  val f: Int => Int = _ * 2
  val g: String => Int = _.length
  val composed = composeGeneric(f, g)

  println(s"Test composeGeneric function passing 'foo' as input. Expected: '6', Result: ${composed("foo")}")
  println(s"Test composeGeneric function passing '' as input. Expected: '0', Result: ${composed("")}")
  println(s"Test composeGeneric function passing 'hello' as input. Expected: '10', Result: ${composed("hello")}")
  println()

  println("Task 6")
  println("Functional compositions - 3 functions")
  println()
  def composeThree[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D = composeGeneric(f, composeGeneric(g, h))

  val h: Int => String = _ + "string"
  val composedThree = composeThree(f, g, h)

  println(s"Test composeThree function passing 3 as input. Expected: '14', Result: ${composedThree(3)}") //(3string).lenght = 7 * 2 =14
  println(s"Test composeThree function passing 10 as input. Expected: '16', Result: ${composedThree(10)}")
  println()

  println("Task 7")
  println("Power of a number using recursion")
  println()

  def power(base: Double, exponent: Int): Double = exponent match
    case 0 => 1.0
    case _ => base * power(base, exponent - 1)

  println(s"Test power function with base 2 and exponent 3. Expected: '8.0', Result: ${power(2, 3)}")
  println(s"Test power function with base 2 and exponent 0. Expected: '1.0', Result: ${power(2, 0)}")

  println()
  println("Power of a number using tail recursion")
  println()

  def tailPower(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def loop(exp: Int, acc: Double): Double = exp match
      case 0 => acc
      case _ => loop(exp - 1, acc * base)
    loop(exponent, 1.0)

  println(s"Test power function with base 2 and exponent 3. Expected: '8.0', Result: ${tailPower(2, 3)}")
  println(s"Test power function with base 2 and exponent 0. Expected: '1.0', Result: ${tailPower(2, 0)}")
  println()

  println("Task 8")
  println("Reverse the digits of an integer using recursion")
  println()
  //reverseNumber(12345) --> 54321
  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def loop(remainingPart: Int, currentReversedNumber: Int): Int = remainingPart match
      case 0 => currentReversedNumber
      case _ => loop(remainingPart/10,(currentReversedNumber*10) +(remainingPart % 10))
    loop(n,0)

  println(s"Test reverseNumber with input 12345. Expected: '54321', Result: ${reverseNumber(12345)}")
  println()



