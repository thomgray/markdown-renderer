package cucumber.steps

import com.gray.util.ImplicitConversions
import cucumber.api.scala.{EN, ScalaDsl}
import org.scalatest.Matchers

class BaseSteps extends ScalaDsl with EN with Matchers with ImplicitConversions {
  val holder = StepHolder
}
