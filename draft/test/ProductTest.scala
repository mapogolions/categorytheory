import io.github.mapogolions.categorytheory._
import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers.allOf
import org.hamcrest.CoreMatchers.anyOf
import org.hamcrest.CoreMatchers.equalTo


class ProductTest {
  @Test
  def limitation(): Unit = {
    // Недостаточный кандидат
    // Test `p1` morphism: Int => Int
    assertEquals( 10, Product.p1(10) )
    // Test `q1` morphisms: Int => Boolean
    assertTrue( Product.q1(-1) )
    assertTrue( Product.q1(200) )
    assertFalse( Product.q1(0) )

    // Proof of limitation. Bottleneck
    // p . m = p1 or q . m = q1. It is always true
    assertEquals(
      (Product.p compose Product.m1)(10),
      Product.p1(10)
    )
    assertEquals(
      (Product.q compose Product.m1)(0),
      Product.q1(0)
    )

    // But vice-versa
    // p1 . m' = p or q1 . m' = q
    // not always is true
    assertNotEquals(
      (Product.q1 compose Product.inverseM1)((0, true)),
      Product.q((0, true))
    )
  }

  @Test
  def redundancy(): Unit = {
    // Избыточный кандидат (Int, Int, Boolean)
    // Test `p2` morphism: (Int, Int, Boolean) => Int
    assertEquals( 10, Product.p2((10, 23, true)) )
    assertEquals( 23, Product.p2((23, 34, true)) )

    // Test `q2` morphism: (Int, Int, Boolean) => Boolean
    assertTrue( Product.q2((34, -45, true)) )
    assertFalse( Product.q2((34, -34, false)) )

    // Test `m2` morphism: (Int, Int, Boolean) => (Int, Boolean)
    assertEquals( Product.m2((10, 45, true)), (10, true) )
    assertEquals( Product.m2((-10, 34, false)), (-10, false) )

    // Test `inverseM2` morphism: (Int, Boolean) => (Int, Int, Boolean)
    assertEquals( Product.inverseM2((10, false)), (10, 0, false) )
    assertEquals( Product.inverseM2((34, true)), (34, 0, true) )

    // Proof of redundancy
    // p . m = p1
    assertEquals(
      (Product.p compose Product.m2)((10, -34, true)),
      Product.p2((10, -34, true))
    )
    // p = p1 . m'
    assertEquals(
      Product.p((10, true)),
      (Product.p2 compose Product.inverseM2)((10, true))
    )
    /** But we have too many morphisms from (Int, Int, Boolean) to Int
     *  We can define banch of function from (Int, Int, Boolean) to Int
     */
  }

  @Test
  def pair(): Unit = {
    // Канонический кандидат. Это пара (Int, Boolean)
    assertTrue( Product.q((10, true)) )
    assertFalse( Product.q((10, false)) )
    assertEquals( 11, Product.p((11, true)) )
    assertEquals( 100, Product.p((100, false)) )
  }
}
