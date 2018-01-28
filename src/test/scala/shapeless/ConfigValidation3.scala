package shapeless

import cc.Spec
import com.typesafe.config.{Config, ConfigFactory}
import cats.data.{NonEmptyList => NEL, Validated}
import cats.implicits._


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


    Then("all validations compose to a triple that can be mapped to a case class instance")


    def res(cfg: Config) = (hasDate(cfg) |@| greater100000(cfg) |@| unchecked(cfg)).map { case (a,b,c) => MyConfig("My config value YYYYMMDD",123456, "anything")}

    res(config1) mustBe Validated.Valid(MyConfig("My config value YYYYMMDD",123456,"anything"))


    When("I have a bad config")
    val config2 = ConfigFactory.parseString(
      """
  my-config {
    foo = My config value YYYYMMD
    bar = 12345
    sftphost = anything
  }
  """)


    res(config2)
    Then("the validations fold into a List[ValueError] with at least one element")
    res(config2) mustBe Validated.Invalid(NEL.of(ValueError("filename should contain a date"), ValueError("int must be greater 100000")))


  }
}
