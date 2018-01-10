package autoconversion

case class Order( customer: Customer, items: List[OrderLineItem]=List()) {
  def addItem( product: Product, quantity: Int ) =
    copy( items = OrderLineItem(product,quantity)::items )
  def total = items.foldLeft(0.0){ _ + _.total }
}

case class Product( name: String, price: Double )

case class OrderLineItem( product: Product, quantity: Int ) {
  def total = quantity * product.price
}

case class Customer( name: String )

case class OrderDto( customerName: String, total: Double )


// The flattening conversion

object Mappings {
  implicit def order2OrderDto( order: Order ) =
    OrderDto( order.customer.name, order.total )
}


object Conversion extends App {


//https://stackoverflow.com/questions/6885558/is-there-something-like-automapper-for-scala
  import autoconversion.Mappings._

  val customer =  Customer( "George Costanza" )
  val bosco = Product( "Bosco", 4.99 )
  val order = Order( customer ).addItem( bosco, 15 )

  val dto: OrderDto = order // automatic conversion at compile-time !

  println( dto ) // prints: OrderDto(George Costanza,74.85000000000001)


}
