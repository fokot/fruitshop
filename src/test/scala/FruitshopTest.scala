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
    "orange" >> {
      Fruitshop.calucatePriceFromString("Orange").get must_== 0.25
    }
    "fruits" >> {
      Fruitshop.calucatePriceFromString("Apple,Apple, Orange Apple\tOrange").get must_== 2.3
    }
  }

}