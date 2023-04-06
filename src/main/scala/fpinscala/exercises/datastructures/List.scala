package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil

  /** Another data constructor, representing nonempty lists. Note that `xs` is
    * another `List[A]`, which may be `Nil` or another `Cons`.
    */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // <--- This will match
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = as match
    case Nil         => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _)
    // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Nil         => sys.error("cannot return tail of Nil list")
    case Cons(x, xs) => xs

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil         => sys.error("cannot set head of Nil list")
    case Cons(x, xs) => Cons(h, xs)

  def drop[A](l: List[A], n: Int): List[A] = l match
    case Nil         => Nil
    case Cons(x, xs) => if n > 0 then drop(xs, n - 1) else l

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Nil         => Nil
    case Cons(x, xs) => if f(x) then dropWhile(xs, f) else l

  // you must always traverse the entire list
  def init[A](l: List[A]): List[A] = l match
    case Nil          => sys.error("cannot call init on Nil list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (a, b) => 1 + b)

  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil         => acc
    case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, (b, a) => a + b)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1, (b, a) => a * b)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (b, a) => 1 + b)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A], (b, a) => Cons(a, b))

  def foldLeftViaFoldRight[A, B](l: List[A], acc: B, f: (B, A) => B): B =
    ???

  def foldRightViaFoldLeft[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    ???

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (a, b) => Cons(a, b))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A], (a, b) => append(a, b))

  def incrementEach(l: List[Int]): List[Int] = ???

  def doubleToString(l: List[Double]): List[String] = ???

  def map[A, B](l: List[A], f: A => B): List[B] = ???

  def filter[A](as: List[A], f: A => Boolean): List[A] = ???

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = ???

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = ???

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = ???

  // def zipWith - TODO determine signature

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
