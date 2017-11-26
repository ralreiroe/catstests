package catstests

import cc.Spec

import scala.util.{Failure, Success, Try}

/**
  * You often have several functions where in a sequence the next function needs the previous function's output as its input.
  * A => B, B => C
  * We can compose those easily.
  *
  * Also, more often than not these functions return a higher kinded type.
  * A => F[B], B => F[C]
  *
  * You want to compose these functions.
  *
  * To do this you need something that takes the inside of F[B] and passes it to the next function.
  *
  *
  * "Libraries that are fully non-blocking with asynchronous I/O operation usually encapsulated their return types in types like futures , one example of such libraries is reactivemongo. When using such libraries you will likely have to use thefuture ‘s flatmap method in composing multiple operations together."
  *
  *
  * Looking carefully at the return types of the service methods(options), we can simply flatmap everything.
val studentGrade = StudentService.getStudentById(id)
                                 .flatMap(getCourses)
                                 .flatMap(getGrades)



  *
  */
class MonadTest extends Spec {

  case class Grade(id:Int, studentId:Int, grade: String)
  case class Student(id: Int, name: String)
  case class StudentCourse(id:Int,
                           studentId: Int,
                           courseId:  Int)
  object StudentService{
    def getStudentById(id: Int): Option[Student] = None
  }
  object StudentCoursesService{
    def getCourses(student: Student): Option[List[StudentCourse]]= None
  }
  object StudentGrades{
    def getGrades(lc: List[StudentCourse]): Option[List[Grade]]= None
  }

  import StudentService._
  import StudentCoursesService._
  import StudentGrades._


  "a" in {


    val studentGrades = getStudentById(5)
      .flatMap(getCourses)
      .flatMap(getGrades).getOrElse(Nil)


    getStudentById(5) match {
      case Some(student) => {
        getCourses(student) match {
          case Some(courses) => {
            getGrades(courses) match {
              case Some(grades) => grades
              case nogrades => Nil
            }
          }
          case nocourses => None
        }
      }
      case nostudent => None

    }


  }

}
