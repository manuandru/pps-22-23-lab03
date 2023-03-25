package exercise

import org.junit.*
import org.junit.Assert.*

class ListTest:
  import exercise.Ex.List.*

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
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))),
      flatMap(list)(v => Cons(v + 1, Cons(v + 2, Nil()))))

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

  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  @Test def testFoldLeft() =
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(-60, foldLeft(list)(0)(_ - _))

  @Test def testFoldRight() =
    assertEquals(-8, foldRight(lst)(0)(_ - _))
    assertEquals(20, foldRight(list)(0)(_ - _))

  // could avoid code duplication, maybe overkilled for 2 line of codes...
  @Test def testFoldRight2() =
    assertEquals(-8, foldRight2(lst)(0)(_ - _))
    assertEquals(20, foldRight2(list)(0)(_ - _))


class PersonTest:
  import exercise.Ex.List.*
  import exercise.Ex.Person.*

  val student = Student("Manuel", 23)
  val course = "PPS"
  val teacher = Teacher("Mirko", course)

  @Test def testNoTeachers() =
    assertEquals(Nil(), findCourses(Nil()))
    assertEquals(Nil(), findCourses(Cons(student, Nil())))

  @Test def testFindCourses() =
    assertEquals(Cons(course, Cons(course, Nil())), findCourses(Cons(teacher, Cons(student, Cons(teacher, Nil())))))
