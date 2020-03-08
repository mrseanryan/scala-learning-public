package list

trait Describable {
  def getDescription(): String
  override def toString(): String = getDescription()
}

// a generic trait (with type parameter T)
// + means is covariant
trait List[+T] extends Describable {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  // Safe prepending via lower-bound:
  // U must be super-type (base) of T, so U is contravariant ('generalizing')
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T] = Nil) extends List[T] {
  def isEmpty: Boolean = false
  override def getDescription(): String = s"${head}, ${tail}"
}

object Nil extends List[Nothing] {
  def isEmpty = true
  // Nothing is a subtype of any other type
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def getDescription(): String = s"."
}

class A {}
class B extends A {}
class C extends A {}

object TestListWithVariance {
  def f(xs: List[B], x: C): List[A] =
    xs prepend x
}
