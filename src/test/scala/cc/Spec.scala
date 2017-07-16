package cc

import org.scalatest.concurrent.ScalaFutures
import org.scalatest._

trait Spec
  extends FreeSpecLike
    with MustMatchers
    with DiagrammedAssertions
    with TryValues
    with OptionValues
    with AppendedClues
    with ScalaFutures
