import java.util.NoSuchElementException
trait Tree[T]{
  def isEmpty : Boolean
  def elem: Int
  def left: Tree[T]
  def right: Tree[T]
}
 class Leaf[T] extends Tree[T]{
  def isEmpty: Boolean = true
  def elem: Nothing = throw new NoSuchElementException("Nil.head")
  def left: Nothing = throw new NoSuchElementException("Nil.head")
  def right: Nothing = throw new NoSuchElementException("Nil.tail")
}

class Branch[T](elem: Int, val left: Leaf[T], val right: Leaf[T] ) extends Tree[T]{
  def isEmpty: Boolean = false
}

def add[T](x: Int): Tree[T]=
if(isEmpty) Branch[T](x)
else if (x < elem) Branch[T](elem, left.add[T](x), right)
else if (x > value)Branch(elem, left, right.add[T](x))
else this

def remove[T](x:Int): Tree[T] =
if (x < elem) Branch(elem,elem, left.add[T](x), right)
else if(x > elem)Branch(elem, left, right.add[T](x))
else {
  if(left.isEmpty && right.isEmpty) Leaf[T]
  else if(left.isEmpty) right
  else if(right.isEmpty) left
  else{
    val succ = right.min
    Branch(succ, left,right.rem(succ))
  }
}

def min: Int = {
def loop (t: Tree[T], m: Int): Int =
if(t.isEmpty) m else loop(t.left, t.elem)

  loop (left, elem)
}

def max: Int = {
  def loop (t:Tree[T], m:Int): Int =
  if(t.isEmpty) m else loop(t.right, t.elem)
loop(right, elem)
}

def apply[T](x: Int):Tree[T] =
val size = left.size
if(x < left.size) left(x)
else if(x > left.size) right(x - size - 1)
else elem

/*
object intsets{
  val t1 = new NonEmpty(3, new Empty, new Empty)
  val t2 = t1 incl 5 incl(4) incl(0) incl(1) incl(10)
}

abstract class Tree {
  def incl(x: Int): Tree
  def contains(x: Int): Boolean
  def union (other: Tree): Tree
  def rem(x: Int): Tree
}


class Empty extends Tree{
  def contains(x: Int): Boolean = false
  def incl(x: Int): Tree = new NonEmpty(x, new Empty, new Empty)
  override def toString = "."
  def union (other: Tree): Tree = other
  def rem(x: Int): Tree = new NonEmpty(x, new Empty, new Empty)
}

class NonEmpty(elem: Int, left: Tree, right: Tree) extends Tree{
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def incl(x: Int): Tree =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  override def toString = "{" + left + elem + right + "}"
  def union (other: Tree): Tree =
    ((left union right) union other) incl elem

}
 class Remove(elem: Int, left: Tree, right: Tree) extends Tree {
   def rem(x: Int): Tree =
   if (x < elem) new NonEmpty(elem, left rem x, right)
   else if (x > elem) new NonEmpty(elem, left, right rem x)
 }

 }*/