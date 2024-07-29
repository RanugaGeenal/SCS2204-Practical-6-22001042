object q1{

  type ProductDetails = (String, Int, Double)

  var inventory1: Map[Int, ProductDetails] = Map(
    10 -> ("ProductA", 10, 99.99),
    20 -> ("ProductB", 5, 199.99),
    30 -> ("ProductC", 20, 49.99)
  )

  var inventory2: Map[Int, ProductDetails] = Map(
    20 -> ("ProductB", 3, 189.99),
    40 -> ("ProductD", 7, 299.99),
    50 -> ("ProductE", 12, 149.99)
  )

  def getAllProductNames(inventory: Map[Int, ProductDetails]): List[String] = {
    inventory.values.map(_._1).toList
  }

  def calculateTotalValue(inventory: Map[Int, ProductDetails]): Double = {
    inventory.values.map { case (_, quantity, price) => quantity * price }.sum
  }

  def isInventoryEmpty(inventory: Map[Int, ProductDetails]): Boolean = {
    inventory.isEmpty
  }

  def mergeInventories(
      inventory1: Map[Int, ProductDetails],
      inventory2: Map[Int, ProductDetails]
  ): Map[Int, ProductDetails] = {
    inventory2.foldLeft(inventory1) {
      case (acc, (id, (name, quantity, price))) =>
        acc.get(id) match {
          case Some((existingName, existingQuantity, existingPrice)) =>
            acc.updated(id, (existingName, existingQuantity + quantity, Math.max(existingPrice, price)))
          case None =>
            acc + (id -> (name, quantity, price))
        }
    }
  }

  def checkAndPrintProductDetails(inventory: Map[Int, ProductDetails], productId: Int): Unit = {
    inventory.get(productId) match {
      case Some((name, quantity, price)) =>
        println(s"Product ID: $productId, Name: $name, Quantity: $quantity, Price: $price")
      case None =>
        println(s"Product with ID $productId does not exist.")
    }
  }

  def main(args: Array[String]): Unit = {
    println("I. Retrieve all product names from inventory1:")
    println(getAllProductNames(inventory1))

    println("\nII. Calculate the total value of all products in inventory1:")
    println(calculateTotalValue(inventory1))

    println("\nIII. Check if inventory1 is empty:")
    println(isInventoryEmpty(inventory1))

    println("\nIV. Merge inventory1 and inventory2:")
    val mergedInventory = mergeInventories(inventory1, inventory2)
    println(mergedInventory)

    println("\nV. Check if product with ID 20 exists and print its details:")
    checkAndPrintProductDetails(inventory1, 20)
  }
}
