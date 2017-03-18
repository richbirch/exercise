import org.scalatest._

object checkout {
  def totalCost(items: List[String]): Double = items.map {
    case "apple" => 0.6
    case "orange" => 0.25
  }.sum
}

class CheckoutSpec extends FlatSpec {

  "checkout" should "return 0 when given an empty list" in {
    val items = List()
    val actualTotal = checkout.totalCost(items)
    val expectedTotal = 0

    assert(actualTotal === expectedTotal)
  }

  "checkout" should "return 0.6 when given a list containing a single apple" in {
    val items = List("apple")
    val actualTotal = checkout.totalCost(items)
    val expectedTotal = 0.6

    assert(actualTotal === expectedTotal)
  }

  "checkout" should "return 1.2 when given a list of two apples" in {
    val items = List("apple", "apple")
    val actualTotal = checkout.totalCost(items)
    val expectedTotal = 1.2

    assert(actualTotal === expectedTotal)
  }

  "checkout" should "return 0.25 when given a list containing one orange" in {
    val items = List("orange")
    val actualTotal = checkout.totalCost(items)
    val expectedTotal = 0.25

    assert(actualTotal === expectedTotal)
  }

  "checkout" should "return 1.7 when given two apples and two oranges" in {
    val items = List("apple", "apple", "orange", "orange")
    val actualTotal = checkout.totalCost(items)
    val expectedTotal = 1.7

    assert(actualTotal === expectedTotal)
  }
}