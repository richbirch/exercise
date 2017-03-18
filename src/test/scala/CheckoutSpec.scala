import org.scalatest._

object checkout {
  def totalCost(items: List[String]): Double = 0
}

class CheckoutSpec extends FlatSpec {

  def testPrices: String => Double = {
    case "apple" => 0.6
    case "orange" => 0.25
  }

  "checkout" should "return 0 when given an empty list" in {
    val items = List()
    val actualTotal = checkout.totalCost(items)
    val expectedTotal = 0

    assert(actualTotal === expectedTotal)
  }
}