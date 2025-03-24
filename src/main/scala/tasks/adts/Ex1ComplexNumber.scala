package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumber:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  import u03.Sequences.*
  import u02.Tuples.*
  object BasicComplexADT extends ComplexADT:
    // Change assignment below: should probably define a case class and use it?
    type Complex = (Double, Double)
    def complex(re: Double, im: Double): Complex = (re, im)
    extension (complex: Complex)
      def re(): Double = complex match
        case (e1, e2) => e1
      def im(): Double = complex match
        case (e1, e2) => e2
      def sum(other: Complex): Complex = (complex, other) match
        case ((e1, e2), (o1, o2)) => (e1 + o1, e2 + o2)
      def subtract(other: Complex): Complex = (complex, other) match
        case ((e1, e2), (o1, o2)) => (e1 - o1, e2 - o2)
      def asString(): String = complex match
        case (e1, e2) if e1 == 0.0 & e2 == 0.0  => 0.0 + "i"
        case (e1, e2) if e1 == 0.0 => 0.0 + "i"
        case (e1, e2) if e2 == 0.0 => 0.0 + "i"
