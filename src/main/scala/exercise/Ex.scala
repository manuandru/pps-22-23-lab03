package exercise

object Ex extends App:

  enum List[A]:
    case Cons(head: A, tail: List[A])
    case Nil()

  object List:
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(_, t) if n > 0 => drop(t, n - 1)
      case _ => l

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Nil() => right
      case Cons(h, t) => Cons(h, append(t, right))

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case _ => Nil()