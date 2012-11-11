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

class ChefContext(val curRecipe: PartialRecipe with RecipeLoopCounter,
                  val recipes: Map[String, PartialRecipe with RecipeLoopCounter],
                  val bowls: ChefStacks = new ChefStacks,
                  val dishes: ChefStacks = new ChefStacks) {
  
  type LoopableRecipe = PartialRecipe with RecipeLoopCounter

  val ingredients = new HashMap[String, Ingredient]
  var curLine = 0
  var terminating = false
  val numOperations = curRecipe.operations.size
  
  implicit def string2ingredient(s: String): Ingredient = ingredients(s)  // this is dangerous
  
  def exec(): Unit = {
    ingredients ++= curRecipe.ingreds.map { (i) ⇒ (i.name, i.copy()) }
    while (curLine < numOperations && !terminating) {
      // println("cur bowl: " + bowls(0) + ", executing: " + curRecipe.operations(curLine))
      process(curRecipe.operations(curLine))
      curLine += 1
    }
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
    case Verb(ingred, verb) ⇒ {
      if (ingred.data == 0) {
        curLine = curRecipe.loopEnds(curLine).get  // assuming no exception thrown
      }
    }
    case VerbUntil(ingred, verbed) ⇒ {
      ingred.map(_.data -= 1)
      curLine = curRecipe.loopStarts(curLine).get - 1  // it will be incremented
    }
    case SetAside() ⇒ {
      curLine = curRecipe.loopEnds(curLine).getOrElse(numOperations)
    }
    case ServeWith(recipe) ⇒ {
      val newBowls = new ChefStacks(bowls)
      val newDishes = new ChefStacks(dishes)
      new ChefContext(recipes(recipe), recipes, newBowls, newDishes).exec()
      // bowls(0).clear() // might be necessary
      bowls(0).pushAll(newBowls(0))
    }
    case Refrigerate(hours) ⇒ {
      hours match {
        case Some(n) ⇒ dishes.showUntil(n)
        case _ ⇒
      }
      terminating = true
    }
    case Serve(num) ⇒ {
      dishes.showUntil(num)
    }
  }
}

class ChefInterpreter {
  def exec(recipe: Recipe): Unit = {
    val callableRecipes = recipe.recipes.mapValues { (r) ⇒
      new PartialRecipe(r.title, r.ingreds, r.operations) with RecipeLoopCounter
    }
    new ChefContext(callableRecipes(recipe.mainRecipe.title), callableRecipes).exec()
  }
}

object ChefInterpreter {
  def main(args: Array[String]) {
    new ChefInterpreter().exec(
      ChefParsers.parseRecipe(io.Source.fromFile(args(0), "UTF-8").getLines mkString "\n")
    )
  }
}
