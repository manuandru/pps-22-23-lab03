package exercise

import scala.annotation.tailrec

object Ex extends App:

  enum List[A]:
    case Cons(head: A, tail: List[A])
    case Nil()

  object List:
    // 1a
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(_, t) if n > 0 => drop(t, n - 1)
      case _ => l

    // 1b
    def append[A](l: List[A], r: List[A]): List[A] = l match
      case Nil() => r
      case Cons(h, t) => Cons(h, append(t, r))

    // 1c
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h), flatMap(t)(f))

    // 1d
    def map[A, B](l: List[A])(f: A => B): List[B] =
      flatMap(l)(e => Cons(f(e), Nil()))

    def filter[A](l: List[A])(p: A => Boolean): List[A] =
      flatMap(l)(e => p(e) match
        case true => Cons(e, Nil())
        case false => Nil()
      )

    // 2
    import u02.Optionals.Option
    import u02.Optionals.Option.*
    def max(l: List[Int]): Option[Int] = l match
      case Nil() => None()
      case Cons(h, t) => max(t) match
        case Some(m) if m > h => Some(m)
        case _ => Some(h)

    // 4
    @tailrec
    def foldLeft[A, B](l: List[A])(acc: B)(op: (B, A) => B ): B = l match
      case Nil() => acc
      case Cons(h, t) => foldLeft(t)(op(acc, h))(op)

    def foldRight[A, B](l: List[A])(acc: B)(op: (A, B) => B): B = l match
      case Nil() => acc
      case Cons(h, t) => op(h, foldRight(t)(acc)(op))

    // foldRight with reverse
    def foldRight2[A, B](l: List[A])(acc: B)(op: (A, B) => B): B =
      @tailrec
      def reverse(l: List[A])(acc: List[A]): List[A] = l match
        case Nil() => acc
        case Cons(h, t) => reverse(t)(Cons(h, acc))
      foldLeft(reverse(l)(Nil()))(acc)((a,b) => op(b,a))



  // 3
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    import List.*
    def findCourses(l: List[Person]): List[String] =
      flatMap(l)(_ match
        case Teacher(_, c) => Cons(c, Nil())
        case _ => Nil()
      )

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = stream match
      case Cons(head, tail) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // 5
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = stream match
      case Cons(_, t) if n > 0 => drop(t())(n - 1)
      case _ => stream

    // 6
    def constant[A](a: A): Stream[A] =
      cons(a, constant(a))

    // 7
    def fibs(): Stream[Int] =
      def fibs(n1: Int, n2: Int): Stream[Int] = cons(n1, fibs(n2, n1 + n2))
      fibs(0, 1)

  end Stream
