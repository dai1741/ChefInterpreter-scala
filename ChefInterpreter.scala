package jp.dai1741.parsing.chef
import scala.util.Random
import scala.collection.mutable.{ HashMap }

import jp.dai1741.parsing.chef.ChefOperations._
import jp.dai1741.parsing.chef.ChefProps._
import jp.dai1741.parsing.chef.ChefParsers

trait RecipeLoopCounter {
  self: PartialRecipe ⇒
  
  private def foldLoop[Pusher: ClassManifest, Popper: ClassManifest]:
      ((List[Int], (Operation, Int)) ⇒ List[Int]) = {
    case (prev, (cur, index)) ⇒ { cur match {
      case o if implicitly[ClassManifest[Pusher]].erasure.isInstance(cur) ⇒ index :: prev
      case o if implicitly[ClassManifest[Popper]].erasure.isInstance(cur) ⇒ prev.tail
      case _ ⇒ prev
    }}
  }
  val (loopStarts, loopEnds) = try {
    (
      operations.zipWithIndex.scanLeft(List.empty[Int]) {
        foldLoop[Verb, VerbUntil] }.init.map(_.headOption),
      operations.zipWithIndex.scanRight(List.empty[Int]) {
        (n,z) ⇒ identity(foldLoop[VerbUntil, Verb])(z,n) }.tail.map(_.headOption)
    )
  } catch {
    case _: UnsupportedOperationException ⇒
      throw new IllegalRecipeException("Imbalanced verb loops")
  }
}

// TODO: Implement, Support auxiliary recipe and verb loops
class ChefInterpreter {
  val bowls = new ChefStacks
  val dishes = new ChefStacks
  var ingredients = new HashMap[String, Ingredient]
  
  implicit def string2ingredient(s: String): Ingredient = ingredients(s)  // this is dangerous
  
  def exec(recipe: Recipe): Unit = {
    val callableRecipes = recipe.recipes.mapValues { (r) ⇒
      new PartialRecipe(r.title, r.ingreds, r.operations) with RecipeLoopCounter
    }
    exec(callableRecipes(recipe.mainRecipe.title), callableRecipes)
  }
  
  def exec(curRecipe: PartialRecipe with RecipeLoopCounter,
           recipes: Map[String, PartialRecipe with RecipeLoopCounter]): Unit = {
    ingredients ++= curRecipe.ingreds.map { (i) ⇒ (i.name, i) }
    for (expr <- curRecipe.operations) process(expr)
  }
  
  def process(expr: Operation) = expr match {
    case FromFridge(ingred) ⇒ {
      ingred.data = readInt // probably Scanner#nextInt is better
    }
    case Put(ingred, bowl) ⇒ {
      bowls(bowl).push(ingred.copy())
    }
    case Fold(ingred, bowl) ⇒ {
      val popped = bowls(bowl).pop
      ingred.data = popped.data
      ingred.iType = popped.iType // maight be unnecessary
    }
    case Add(ingred, bowl) ⇒ {
      bowls(bowl).last.data += ingred.data
    }
    case Remove(ingred, bowl) ⇒ {
      bowls(bowl).last.data -= ingred.data
    }
    case Combine(ingred, bowl) ⇒ {
      bowls(bowl).last.data *= ingred.data
    }
    case Divide(ingred, bowl) ⇒ {
      bowls(bowl).last.data /= ingred.data // should reverse divisor and dividend?
    }
    case AddDries(bowl) ⇒ {
      // TODO: Retain ingredients ordering
      ingredients.values.filter(_.iType == dry).foreach(bowls(bowl).push(_))
    }
    case LiquefyIngredient(ingred) ⇒ {
      ingred.liquefy
    }
    case LiquefyContents(bowl) ⇒ {
      bowls(bowl).foreach(_.liquefy)
    }
    case StirWhile(bowl, minutes) ⇒ {
      bowls(bowl).stir(minutes)
    }
    case StirWith(ingred, bowl) ⇒ {
      bowls(bowl).stir(ingred.data)
    }
    case Mix(bowl) ⇒ {
      // whys there no in-place shuffle?
      val shuffled = Random.shuffle(bowls(bowl).toSeq)
      bowls(bowl).clear()
      bowls(bowl).pushAll(shuffled)
    }
    case Clean(bowl) ⇒ {
      bowls(bowl).clear()
    }
    case Pour(bowl, dish) ⇒ {
      dishes(dish).pushAll(bowls(bowl))
    }
    case Verb(verb, ingred) ⇒ {
    }
    case VerbUntil(ingred, verbed) ⇒ {
    }
    case SetAside() ⇒ {
    }
    case ServeWith(recipe) ⇒ {
    }
    case Refrigerate(hours) ⇒ {
      hours match {
        case Some(n) ⇒ dishes.showUntil(n)
        case _ ⇒
      }
      sys.exit(0) // TODO: auxiliary-recipe
    }
    case Serve(num) ⇒ {
      dishes.showUntil(num)
    }
  }
}

object ChefInterpreter {
  def main(args: Array[String]) {
    new ChefInterpreter().exec(
      ChefParsers.parseRecipe(io.Source.fromFile(args(0), "UTF-8").getLines mkString "\n")
    )
  }
}
