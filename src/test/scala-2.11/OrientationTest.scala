import org.scalatest.{FlatSpec, Matchers}

class OrientationTest extends FlatSpec with Matchers {
  "Orientation" should "change its orientation on command left" in {
    North.left should be (West)
    East.left should be (North)
    West.left should be (South)
    South.left should be (East)
  }

  it should "change its orientation on command right" in {
    North.right should be (East)
    East.right should be (South)
    West.right should be (North)
    South.right should be (West)
  }
}
