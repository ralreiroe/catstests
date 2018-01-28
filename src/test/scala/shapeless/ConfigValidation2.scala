package shapeless

import cc.Spec
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.GivenWhenThen

class ConfigValidation2 extends Spec with GivenWhenThen {

  //https://www.cakesolutions.net/teamblogs/using-shapeless-to-validate-typesafe-configuration-data

//  The Typesafe config library offers no tools for performing this type of validation.

  def buildUnsafe[T](
                                validatedParams: (Config => Either[ValueError, Any])*
                                )(config: Config)(implicit gen: Generic[T]
                                ): Either[List[ValueError], T] = {
    val failuresHList: (List[ValueError], HList) =
      validatedParams.map(_.apply(config)).foldRight[(List[ValueError], HList)]((Nil, HNil)) {
        case (Left(error), (failures, result)) =>
          (error +: failures, result)
        case (Right(value), (failures, result)) =>
          (failures, value :: result)
      }

    failuresHList match {
      // FIXME: the following appears to lead to unnecessary runtime errors!
      case (Nil, result: (gen.Repr @unchecked)) =>
        Right(gen.from(result))
      case (failures, _) =>
        Left(failures)
    }
  }

  "config validation of a typesafe config with Shapeless Generic with a case class that cannot be instantiated outside this package" in {

    Given("an abstract sealed case class representing the config... a sealed abstract case classes has compiler to build case class niceness such as field val's, hash function, equality and unapply - but omits creating any constructors.")
    sealed abstract case class HttpConfig(host: String, port: Int)
    And("the ability to create an instance of it...")
    implicit val genHttpConfig: Generic[HttpConfig] = new Generic[HttpConfig] {
      type Repr = String :: Int :: HNil

      def to(t: HttpConfig): Repr = t.host :: t.port :: HNil
      def from(r: Repr): HttpConfig =
        new HttpConfig(r(0), r(1)) {}
    }
    And("the validation functions")
    val hasDate: Config => Either[ValueError, String] = cfg => {
      if (cfg.getString("my-config.foo").contains("YYYYMMDD")) Right(cfg.getString("my-config.foo")) else Left(ValueError("filename should contain a date"))
    }
    val greater100000: Config => Either[ValueError, Int] = cfg => {
      if (cfg.getInt("my-config.bar") > 100000) Right(cfg.getInt("my-config.bar")) else Left(ValueError("int must be greater 100000"))
    }
    val unchecked: Config => Either[ValueError, String] = cfg => Right(cfg.getString("my-config.sftphost"))

    When("I have a good config")
    val config1 = ConfigFactory.parseString(
      """
  my-config {
    foo = My config value YYYYMMDD
    bar = 123456
    sftphost = anything
  }
  """)

    Then("all validations fold into an HList that is convertible into a case class instance")
    buildUnsafe(hasDate,greater100000,unchecked)(config1) mustBe Right(new HttpConfig("My config value YYYYMMDD",123456) {})



  }
}
