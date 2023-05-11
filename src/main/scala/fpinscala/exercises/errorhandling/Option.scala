package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option as _, Some as _, None as _}
import scala.collection.immutable.Nil

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case Some(get) => Some(f(get))
    case None      => None

  def getOrElse[B >: A](default: => B): B = this match
    case Some(get) => get
    case None      => default

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    if this.map(f).getOrElse(false) then this
    else None
    // this.flatMap(el => if f(el) then Some(el) else None)
object Option:

  def failingFn(i: Int): Int =
    val y: Int =
      throw new Exception(
        "fail!"
      ) // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch
      case e: Exception =>
        43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception(
        "fail!"
      )): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))
    // (a, b) match
    //   case (None, _)            => None
    //   case (_, None)            => None
    //   case (Some(aa), Some(bb)) => Some(f(aa, bb))

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(a => a)
    // as match
    //   case head :: tail => head.flatMap(h => sequence(tail).map(l => h :: l))
    //   case Nil          => Some(Nil)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(List[B]()))((curr, prev) =>
      prev.flatMap(p => f(curr).map(c => c :: p))
    )
