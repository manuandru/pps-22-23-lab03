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

  @Test def testEdgeCaseAppend() =
    assertEquals(list, append(list, Nil()))
    assertEquals(list, append(Nil(), list))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(list)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(list)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(list)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(list)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(list)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(list)(_ != 20))
    assertEquals(Nil(), filter(list)(_ < 0))

  import u02.Optionals.Option.*
  @Test def testMax() =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(None(), max(Nil()))

  import exercise.Ex.Person.*
  @Test def testNoTeachers() =
    assertEquals(Nil(), findCourses(Nil()))
    val student = Student("Manuel", 23)
    assertEquals(Nil(), findCourses(Cons(student, Nil())))

  @Test def testFindCourses() =
    val student = Student("Manuel", 23)
    val pps = "PPS"
    val teacher = Teacher("Mirko", pps)
    assertEquals(Cons(pps, Cons(pps, Nil())), findCourses(Cons(teacher, Cons(student, Cons(teacher, Nil())))))
