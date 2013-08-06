package jp.dai1741.parsing.chef

import scala.util.Random
import scala.collection.mutable.{ ArrayBuffer, HashMap }
  
class ChefStacks extends HashMap[Int, ChefStack] {
  def this(stacks: ChefStacks) {
    this()
    for ((i, stack) ← stacks) {
      this += i → new ChefStack(stack)
    }
  }
  
  override def apply(i: Int) = {
    getOrElseUpdate(i, new ChefStack)
  }
  def show(n: Int) {
    for (i ← 0 until n) {
      print(this(i).toSeq.reverse.map(_.toString) mkString "")
    }
    // should print line after each dish? or rather shouldn't print line at all?
    // println()
  }
}

class ChefStack extends ArrayBuffer[Ingredient] {
  def this(stack: ChefStack) {
    this
    for (m ← stack) this += m.copy()
  }
  
  def push(elem: Ingredient) = this += elem
  def pushAll(that: TraversableOnce[Ingredient]) = this ++= that
  
  def pop = {
    val ret = last
    trimEnd(1)
    ret
  }
  
  def stir(num: Int) = insert(math.max(size - num - 1, 0), pop)
  def shuffle {
    val shuffled = Random.shuffle(this.toSeq)
    this.clear()
    this ++= shuffled
  }
}
