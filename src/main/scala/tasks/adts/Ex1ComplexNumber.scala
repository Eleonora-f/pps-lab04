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
    def complex(re: Double, im: Double): Complex =
      (re, im)
    extension (complex: Complex)
      def re(): Double = complex match
        case (e1, e2) => e1 //prendo solo il primo dei due
      def im(): Double = complex match
        case (e1, e2) => e2 //prendo solo il secondo dei due
      def sum(other: Complex): Complex = (complex, other) match
        case ((e1, e2), (o1, o2)) => (e1 + o1, e2 + o2)
      def subtract(other: Complex): Complex = (complex, other) match
        case ((e1, e2), (o1, o2)) => (e1 - o1, e2 - o2)
      def asString(): String = complex match
        case (e1, e2) if e1 == 0.0 && e2 == 0.0  => "0.0" //num complex=0 restituisce 0
        case (e1, e2) if e1 == 0.0 => e2 + "i"//p. reale=0 è un numero immaginario puro
        case (e1, e2) if e2 == 0.0 => e1 + ""//numero reale pure, se la p. imm=0
        case (e1, e2) if e2 < 0.0 => e1 + " - " + (-e2) + "i" //N. complex con p. imm negativa(a - bi)
        case (e1, e2) => e1 + " + " + e2 + "i" //a + bi
