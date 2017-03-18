import org.scalatest._

trait Offer {
  def applyOffer(items: List[String]): List[String]
}

object applesTwoForOne extends Offer {
  def applyOffer(items: List[String]): List[String] = {
    checkout.oneFreeFor("apple", 2)(items)
  }
}

object orangesThreeForTwo extends Offer {
  def applyOffer(items: List[String]): List[String] = {
    checkout.oneFreeFor("orange", 3)(items)
  }
}

object checkout {
  def totalCost(offers: List[Offer])(items: List[String]): Double = {
    offers
      .foldLeft(items)((acc, offer) => offer.applyOffer(acc))
      .map {
        case "apple" => 0.6
        case "orange" => 0.25
      }.sum
  }

  def oneFreeFor(itemType: String, offer: Int)(items: List[String]): List[String] = {
    items.foldLeft((List[String](), 0))((acc, f) => (acc, f) match {
      case ((items, count), item) if item == itemType =>
        if (count + 1 < offer) (f :: items, count + 1)
        else (items, 0)
      case ((items, count), _) => (f :: items, count)
    })._1
  }
}


class CheckoutSpec extends FlatSpec {
  def testTotalCostWithNoOffers = checkout.totalCost(List()) _

  def testTotalCostWithOffers = checkout.totalCost(List(applesTwoForOne, orangesThreeForTwo)) _

  "checkout" should "return 0 when given an empty list" in {
    val items = List()
    val actualTotal = testTotalCostWithNoOffers(items)
    val expectedTotal = 0

    assert(actualTotal === expectedTotal)
  }

  "checkout" should "return 0.6 when given a list containing a single apple" in {
    val items = List("apple")
    val actualTotal = testTotalCostWithNoOffers(items)
    val expectedTotal = 0.6

    assert(actualTotal === expectedTotal)
  }

  "checkout" should "return 1.2 when given a list of two apples" in {
    val items = List("apple", "apple")
    val actualTotal = testTotalCostWithNoOffers(items)
    val expectedTotal = 1.2

    assert(actualTotal === expectedTotal)
  }

  "checkout" should "return 0.25 when given a list containing one orange" in {
    val items = List("orange")
    val actualTotal = testTotalCostWithNoOffers(items)
    val expectedTotal = 0.25

    assert(actualTotal === expectedTotal)
  }

  "checkout" should "return 1.7 when given two apples and two oranges" in {
    val items = List("apple", "apple", "orange", "orange")
    val actualTotal = testTotalCostWithNoOffers(items)
    val expectedTotal = 1.7

    assert(actualTotal === expectedTotal)
  }

  "applesTwoForOne" should "return 2 oranges when given 2 oranges" in {
    val items = List[String]("orange", "orange")
    val actualItems = applesTwoForOne.applyOffer(items)
    val expectedItems = List("orange", "orange")

    assert(actualItems === expectedItems)
  }

  "applesTwoForOne" should "return 1 apple when given 2 apples" in {
    val items = List[String]("apple", "apple")
    val actualItems = applesTwoForOne.applyOffer(items)
    val expectedItems = List("apple")

    assert(actualItems === expectedItems)
  }

  "checkout" should "return 0.6 when given a list containing two apples" in {
    val items = List("apple", "apple")
    val actualTotal = testTotalCostWithOffers(items)
    val expectedTotal = 0.6

    assert(actualTotal === expectedTotal)
  }

  "checkout" should "return 1.1 when given a list containing two apples and three oranges" in {
    val items = List("apple", "apple", "orange", "orange", "orange")
    val actualTotal = testTotalCostWithOffers(items)
    val expectedTotal = 1.1

    assert(actualTotal === expectedTotal)
  }

  "checkout" should "return 1.2 + 0.75 = 1.95 when given a list containing three apples and four oranges" in {
    val items = List("apple", "apple", "apple", "orange", "orange", "orange", "orange")
    val actualTotal = testTotalCostWithOffers(items)
    val expectedTotal = 1.95

    assert(actualTotal === expectedTotal)
  }
}