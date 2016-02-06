

object Fruitshop {

  object Product extends Enumeration {
    val Apple, Orange = Value
    val stingValues = values.map(_.toString)
  }


  def parse(products: String) : Option[List[Product.Value]] = {
    val splitted = products.split("(\\s*,\\s*|\\s+)").toList
    val v = !splitted.forall(Product.stingValues.contains)
    if(splitted.forall(Product.stingValues.contains))
      Some(splitted map Product.withName)
    else
      None
  }

  import Product._
  def prices(product: Product.Value, n: Int) = product match {
    case Apple => Math.floor(n / 2) * 0.6 + (n % 2) * 0.6
    case Orange => Math.floor(n / 3) * 0.5 + (n % 3) * 0.25
  }

  def calucatePrice(products: List[Product.Value]) = {
    products.groupBy(identity).mapValues(_.size).map((prices _).tupled).sum
  }
  def calucatePriceFromString(products: String) = parse(products).map(calucatePrice)
  def printPrice(products: String) =
    println(calucatePriceFromString(products).map("Price is " + _).getOrElse("Can't parse products!"))
}