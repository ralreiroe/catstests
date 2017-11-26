package catstests

import cc.Spec

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


  "" in {


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
