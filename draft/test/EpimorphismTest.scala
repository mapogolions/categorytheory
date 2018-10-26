import io.github.mapogolions.categorytheory._
import org.junit.Test
import org.junit.Assert._


class EpimorphismTest {
  @Test
  def proveTest(): Unit = {
    assertTrue(Epimorphism.firstConditionSatisfied)
    assertTrue(Epimorphism.secondConditionSatisfied)
    assertTrue(Epimorphism.thirdConditionSatisfied)
  }
}
