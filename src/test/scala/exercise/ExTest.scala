package exercise

import exercise.Ex.List.*
import org.junit.*
import org.junit.Assert.*

class ExTest:

  val list = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(list, 1))
    assertEquals(Cons(30, Nil()), drop(list, 2))
    assertEquals(Nil(), drop(list, 3))

  @Test def testEdgeCaseDrop() =
    assertEquals(list, drop(list, 0))
    assertEquals(list, drop(list, -3))

  @Test def testAppend() =
    val tail = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), append(list, tail))


