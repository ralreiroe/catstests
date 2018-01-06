package typeclasses

package object TypeClassHtmlWriterTest7HtmlOperations {

  def toHtml4[A](value: A)(implicit w: A => String) = w(value)


}