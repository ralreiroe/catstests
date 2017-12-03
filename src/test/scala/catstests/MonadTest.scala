package catstests

import cc.Spec

import scala.concurrent.Future
import scala.util.control.NonFatal
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


  "option composition without flatmap" in {

    /**
      * Option[A]
      * def flatMap[B](f: A => Option[B]): Option[B] =
    if (isEmpty) None else f(this.get)
      */


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

  "future" in {

    import scala.concurrent.ExecutionContext.Implicits.global

    def funcOnInt(i: Int) = Future(i)


    /**
      *
      * trait Future[+T] extends Awaitable[T] {
      *   def flatMap[S](f: T => Future[S])(implicit executor: ExecutionContext): Future[S] = transformWith {
    case Success(s) => f(s)
    case Failure(_) => this.asInstanceOf[Future[S]]
  }

  override def transformWith[S](f: Try[T] => Future[S])(implicit executor: ExecutionContext): Future[S] = {
    val p = new DefaultPromise[S]()
    onComplete {
      v => try f(v) match {
        case fut if fut eq this => p complete v.asInstanceOf[Try[S]]
        case dp: DefaultPromise[_] => dp.asInstanceOf[DefaultPromise[S]].linkRootOf(p)
        case fut => p completeWith fut
      } catch { case NonFatal(t) => p failure t }
    }
    p.future
  }


    def onComplete[U](func: Try[T] => U)(implicit executor: ExecutionContext): Unit = {
      val completedAs = value.get
      val preparedEC = executor.prepare
      (new CallbackRunnable(preparedEC, func)).executeWithValue(completedAs)
    }
      */

  }

  "try" in {

    /**
      *final case class Success[+T](value: T) extends Try[T] {
      *     try f(value) catch { case NonFatal(e) => Failure(e) }

      */

  }

  "either" in {

    /**
      *   def flatMap[AA >: A, Y](f: B => Either[AA, Y]): Either[AA, Y] = this match {
    case Right(b) => f(b)
    case Left(a)  => this.asInstanceOf[Either[AA, Y]]
  }
      */
  }

  "list" in {

    /**
      *   def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    def builder = bf(repr) // extracted to keep method size under 35 bytes, so that it can be JIT-inlined
    val b = builder
    for (x <- this) b ++= f(x).seq      //add result elements to new collection b
    b.result
  }
      */
  }

}
