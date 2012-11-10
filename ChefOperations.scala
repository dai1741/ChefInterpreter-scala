package jp.dai1741.parsing.chef
  
object ChefOperations {

  trait Operation
  
  case class FromFridge(ingredient: String) extends Operation
  case class Put(ingredient: String, bowl: Int) extends Operation
  case class Fold(ingredient: String, bowl: Int) extends Operation
  case class Add(ingredient: String, bowl: Int) extends Operation
  case class Remove(ingredient: String, bowl: Int) extends Operation
  case class Combine(ingredient: String, bowl: Int) extends Operation
  case class Divide(ingredient: String, bowl: Int) extends Operation
  case class AddDries(bowl: Int) extends Operation
  case class LiquefyIngredient(ingredient: String) extends Operation
  case class LiquefyContents(bowl: Int) extends Operation
  case class StirWhile(bowl: Int, minutes: Int) extends Operation
  case class StirWith(ingredient: String, bowl: Int) extends Operation
  case class Mix(bowl: Int) extends Operation
  case class Clean(bowl: Int) extends Operation
  case class Pour(bowl: Int, dish: Int) extends Operation
  case class Verb(ingredient: Option[String], verb: String) extends Operation
  case class VerbUntil(ingredient: Option[String], verbed: String) extends Operation
  case class SetAside() extends Operation
  case class ServeWith(recipe: String) extends Operation
  case class Refrigerate(hours: Option[Int]) extends Operation
  
  case class Serve(num: Int) extends Operation
}
