import io.github.mapogolions.categorytheory._
import org.junit.Test
import org.junit.Assert._


class MonomorphismTest {
  @Test
  def proveTest(): Unit = {
    assertTrue(Monomorphism.firstConditionSatisfied)
    assertTrue(Monomorphism.secondConditionSatisfied)
    assertTrue(Monomorphism.thirdConditionSatisfied)
  }
}
