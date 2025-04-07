package ex1

import scala.::

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)  // pattern for scala.Option
    case _ => None          // pattern for scala.Option

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None
  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def append(list: List[A]): List[A] =
    foldRight(list)(_ :: _)

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight(Nil())(f(_) append _)

  def filter(predicate: A => Boolean): List[A] = flatMap(a => if predicate(a) then a :: Nil() else Nil())

  def map[B](fun: A => B): List[B] = flatMap(a => fun(a) :: Nil())

  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new IllegalStateException()
    case h :: t => t.foldLeft(h)(op)

  // Exercise: implement the following methods
  def foldLeft[B](init: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(init, h))(op)
    case _ => init

  def foldRight[B](init: B)(op: (A, B) => B): B = this match
    case h :: t => op(h, t.foldRight(init)(op))
    case _ => init

  def zipWithValue[B](value: B): List[(A, B)] = foldRight(Nil())((_, value) :: _)
  def length(): Int = foldLeft(0)((b, _) => b + 1)
  def zipWithChangingValue[B](init: B)(op: B => B): List[(A, B)] = 
    foldRight((Nil[(A, B)](), init))((a, b) => ((a, b._2) :: b._1, op(b._2)))._1
  def zipWithIndex: List[(A, Int)] = zipWithChangingValue(this.length() - 1)(_ - 1)
  def zipWithIndexIter: List[(A, Int)] =
    def iter(list: List[A], index: Int): List[(A, Int)] = list match
      case h :: t => (h, index) :: iter(t, index + 1)
      case _ => Nil()
    iter(this, 0)
  def partitionWithFilter(predicate: A => Boolean): (List[A], List[A]) = (filter(predicate), filter(a => !predicate(a)))
  def partition(predicate: A => Boolean): (List[A], List[A]) = foldRight((Nil(), Nil()))((a, b) => 
    if predicate(a) then (a :: b._1, b._2) else (b._1, a :: b._2))
  def span(predicate: A => Boolean): (List[A], List[A]) = ???
  def takeRight(n: Int): List[A] = zipWithChangingValue(0)(_ + 1).filter((_, i) => i < n).map((a, _) => a)
  def collect(predicate: PartialFunction[A, A]): List[A] = ???
// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

object Test extends App:

  import List.*
  val reference = List(10, 20, 30, 40)
  println(reference)
  println(reference.zipWithValue(10)) // List((1, 10), (2, 10), (3, 10), (4, 10))
  println(reference.length())
  println(reference.zipWithIndex) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.partitionWithFilter(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.reduce(_ + _)) // 10
  println(List(10).reduce(_ + _)) // 10
  println(reference.takeRight(3)) // List(2, 3, 4)
  println(reference.collect { case x if x % 2 == 0 => x + 1 }) // List(3, 5)