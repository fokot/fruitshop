import org.specs2.mutable.Specification
import  Fruitshop.Product._

class FruitshopTest extends Specification {

  "Fruitshop.parse" >> {

    val expected = List(Apple, Apple, Orange, Apple, Orange)

    "apple" >> {
      Fruitshop.parse("Apple").get must_== List(Apple)
    }
    "orange" >> {
      Fruitshop.parse("Orange").get must_== List(Orange)
    }
    "spaces" >> {
      Fruitshop.parse("Apple Apple Orange    Apple\tOrange").get must_== expected
    }
    "commas" >> {
      Fruitshop.parse("Apple,Apple, Orange ,Apple , Orange").get must_== expected
    }
    "mixed" >> {
      Fruitshop.parse("Apple,Apple, Orange Apple\tOrange").get must_== expected
    }
  }

  "Fruitshop.calucatePriceFromString" >> {


    "apple" >> {
        Fruitshop.calucatePriceFromString("Apple").get must_== 0.6
      }
    "2 apples" >> {
        Fruitshop.calucatePriceFromString("Apple Apple").get must_== 0.6
      }
    "3 apples" >> {
        Fruitshop.calucatePriceFromString("Apple Apple Apple").get must_== 1.2
      }
    "orange" >> {
        Fruitshop.calucatePriceFromString("Orange").get must_== 0.25
      }
    "2 oranges" >> {
        Fruitshop.calucatePriceFromString("Orange Orange").get must_== 0.5
      }
    "3 oranges" >> {
        Fruitshop.calucatePriceFromString("Orange Orange Orange").get must_== 0.5
      }
    "4 oranges" >> {
        Fruitshop.calucatePriceFromString("Orange Orange Orange Orange").get must_== 0.75
      }
    "fruits" >> {
        Fruitshop.calucatePriceFromString("Apple,Apple, Orange Apple\tOrange").get must_== 1.7
      }
  }
}