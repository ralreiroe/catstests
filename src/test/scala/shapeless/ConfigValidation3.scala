package shapeless

import cc.Spec
import com.typesafe.config.{Config, ConfigFactory}
import cats.data.{NonEmptyList => NEL, Validated}

class ConfigValidation3 extends Spec {

  //https://www.cakesolutions.net/teamblogs/using-shapeless-to-validate-typesafe-configuration-data

//  The Typesafe config library offers no tools for performing this type of validation.


  "config validation of a typesafe config with Shapeless Generic" in {

    Given("the case class representing the config...")
    case class MyConfig(foo: String, bar: Int, sftphost: String)
    And("the validation functions")
    val hasDate: Config => Validated[NEL[ValueError], String] = cfg => {
      if (cfg.getString("my-config.foo").contains("YYYYMMDD")) Validated.Valid(cfg.getString("my-config.foo")) else Validated.Invalid(NEL.of(ValueError("filename should contain a date")))
    }
    val greater100000: Config => Validated[NEL[ValueError], Int] = cfg => {
      if (cfg.getInt("my-config.bar") > 100000) Validated.Valid(cfg.getInt("my-config.bar")) else Validated.Invalid(NEL.of(ValueError("int must be greater 100000")))
    }
    val unchecked: Config => Validated[NEL[ValueError], String] = cfg => Validated.Valid(cfg.getString("my-config.sftphost"))

    When("I have a good config")
    val config1 = ConfigFactory.parseString(
      """
  my-config {
    foo = My config value YYYYMMDD
    bar = 123456
    sftphost = anything
  }
  """)

    import cats.syntax.cartesian._

    import cats.implicits._


    Then("all validations compose to a triple that can be mapped to a case class instance")
    val res2 = (cfg: Config) => (hasDate(cfg) |@| greater100000(cfg) |@| unchecked(cfg)).map { case (a,b,c) => MyConfig("My config value YYYYMMDD",123456, "anything")}

    When("I have a bad config")
    val config2 = ConfigFactory.parseString(
      """
  my-config {
    foo = My config value YYYYMMD
    bar = 12345
  }
  """)

    println(res2(config1))
    println(res2(config2))

    Then("the validations fold into a List[ValueError] with at least one element")
//    buildUnsafe(hasDate,greater100000)(config2) mustBe Left(List(ValueError("filename should contain a date"), ValueError("int must be greater 100000")))


  }
}
