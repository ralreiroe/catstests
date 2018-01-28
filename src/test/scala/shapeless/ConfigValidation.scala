package shapeless

import cc.Spec
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.GivenWhenThen

case class ValueError(reason: String)

class ConfigValidation extends Spec with GivenWhenThen {

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

  "config validation of a typesafe config with Shapeless Generic" in {

    Given("the case class representing the config...")
    case class MyConfig(foo: String, bar: Int, sftphost: String)
    And("the ability to create an instance of it...")
    implicit val gen = Generic[MyConfig]
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
    buildUnsafe(hasDate,greater100000,unchecked)(config1) mustBe Right(MyConfig("My config value YYYYMMDD",123456, "anything"))

    When("I have a bad config")
    val config2 = ConfigFactory.parseString(
      """
  my-config {
    foo = My config value YYYYMMD
    bar = 12345
  }
  """)

    Then("the validations fold into a List[ValueError] with at least one element")
    buildUnsafe(hasDate,greater100000)(config2) mustBe Left(List(ValueError("filename should contain a date"), ValueError("int must be greater 100000")))


  }
}
