package exercise

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
