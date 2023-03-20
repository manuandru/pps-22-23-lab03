package exercise

object Ex extends App:

  enum List[A]:
    case Cons(head: A, tail: List[A])
    case Nil()

  object List:
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Cons(_, t), i) if i > 0 => drop(t, i - 1)
      case _ => l

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, Nil()) => Cons(h, right)
      case Cons(h, t) => Cons(h, append(t, right))