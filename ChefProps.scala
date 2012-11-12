package jp.dai1741.parsing.chef

import scala.collection.mutable.{ ArrayBuffer, HashMap }
import jp.dai1741.parsing.chef.ChefOperations._
  
object ChefProps {
  
  class PartialRecipe(val title: String,
                      val ingreds: List[Ingredient],
                      val operations: List[Operation])
  
  class Recipe(val mainRecipe: PartialRecipe,
               val recipes: Map[String, PartialRecipe])
  
  case class Ingredient(val name: String,
                        var iType: IngredientType,
                        private var _data: Int = -1,
                        var isDefined: Boolean = false) {
    def data_=(n: Int) = {
      _data = n
      isDefined = true
    }
    def data = {
      if (!isDefined) throw new IngredientNotInitializedException
      _data
    }
    def liquefy { iType = liquid }
    override def toString = iType.convertData(_data)
  }
  
  class IngredientNotInitializedException extends IllegalStateException
  
  class IllegalRecipeException(msg: String) extends IllegalArgumentException(msg)
  
  // Scala's enum is pretty cool, though not useful than java's one i think
  case class IngredientType(convertData: (Int) ⇒ String)
  val dry = IngredientType(_.toString + " ")
  val liquid = IngredientType(_.toChar.toString)
  
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
    def showUntil(n: Int) {
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
  }
}
