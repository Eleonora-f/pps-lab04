package tasks.typeclasses
import u03.Sequences.* 
import Sequence.*
import u03.Optionals.*
import Optional.*

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: "+a)

  def logAll[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAll(t)
    case _ => ()

  trait Traversable[T[_]]:
    def traverse[A](t: T[A]): Unit

  given Traversable[Optional] with
    //restituisce il valore di Optional oppure empty se è vuoto
    def traverse[A](t: Optional[A]): Unit = log(orElse(t, "Empty"))

  given Traversable[Sequence] with
    def traverse[A](s: Sequence[A]): Unit = logAll(s)
  
  def logAll[T[_]: Traversable, A](x: T[A]): Unit =
    summon[Traversable[T]].traverse(x)

  @main def tryTraversable =
    val seq = Cons("one", Cons("two", Cons("three", Nil())))
    logAll(seq)

    val opt = Optional.Just(5)
    logAll(opt)

    val empty = Empty()
    logAll(empty)

  
