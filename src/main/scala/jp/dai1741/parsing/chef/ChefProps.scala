package jp.dai1741.parsing.chef

class PartialRecipe(val title: String,
                    val ingreds: List[Ingredient],
                    val operations: List[Operation]) {
  
  def this(p: PartialRecipe) {
    this(p.title, p.ingreds, p.operations)
  }
}

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
  def liquefy { iType = IngredientType.liquid }
  override def toString = iType.convertData(_data)
}

class IngredientNotInitializedException extends IllegalStateException

class IllegalRecipeException(msg: String) extends IllegalArgumentException(msg)

// Scala's enum is pretty cool, though not useful than java's one i think
class IngredientType(val convertData: (Int) ⇒ String)

object IngredientType {
  val dry = new IngredientType(_.toString + " ")
  val liquid = new IngredientType(_.toChar.toString)
}