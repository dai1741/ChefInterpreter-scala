package jp.dai1741.parsing.chef

import jp.dai1741.parsing.chef.ChefOperations._

trait ChefInterpreter {
  def execute(recipe: Recipe)
}

trait LoopablePartialRecipe {
  val ingreds: List[Ingredient]
  val operations: List[Operation]
  val loopStarts: List[Option[Int]]
  val loopEnds: List[Option[Int]]
}

class ChefRuntimeException(msg: String, cause: Throwable) extends RuntimeException(msg, cause)

class ChefContext(val curRecipe: LoopablePartialRecipe,
                  val recipes: Map[String, LoopablePartialRecipe],
                  val bowls: ChefStacks = new ChefStacks,
                  val dishes: ChefStacks = new ChefStacks) {

  val ingredients = Map(curRecipe.ingreds.map { (i) ⇒ (i.name, i.copy()) }: _*)
  // hmm i cant understand why the below code wont work
  // val ingredients = curRecipe.ingreds.groupBy(_.name).mapValues(_.last.copy())
  
  var curLine = 0
  val numOperations = curRecipe.operations.size
  
  implicit def string2ingredient(s: String): Ingredient = ingredients(s)  // this is dangerous
  
  def execute(): Unit = {
    try {
      while (curLine < numOperations) {
        process(curRecipe.operations(curLine))
        curLine += 1
      }
    } catch {
      case e: Exception => throw new ChefRuntimeException(
          "Exception caused when executing %s (operation #%d)".format(
            curRecipe.operations(curLine), curLine), e)
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
      ingred.data = bowls(bowl).pop.data
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
      bowls(bowl).last.data /= ingred.data
    }
    case AddDries(bowl) ⇒ {
      bowls(bowl).push(
        Ingredient("dummy", IngredientType.dry, ingredients.values.filter(
          _.iType == IngredientType.dry).map(_.data).sum, true))
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
      bowls(bowl).shuffle
    }
    case Clean(bowl) ⇒ {
      bowls(bowl).clear()
    }
    case Pour(bowl, dish) ⇒ {
      dishes(dish).pushAll(bowls(bowl))
    }
    case Verb(ingred, verb) ⇒ {
      if (ingred.data == 0) {
        curLine = curRecipe.loopEnds(curLine).getOrElse(curLine)
      }
    }
    case VerbUntil(ingred, verbed) ⇒ {
      ingred.foreach(_.data -= 1)
      curLine = curRecipe.loopStarts(curLine).map(_ - 1).getOrElse(curLine)
    }
    case SetAside() ⇒ {
      curLine = curRecipe.loopEnds(curLine).getOrElse(numOperations)
    }
    case ServeWith(recipe) ⇒ {
      val newBowls = new ChefStacks(bowls)
      val newDishes = new ChefStacks(dishes)
      new ChefContext(recipes(recipe), recipes, newBowls, newDishes).execute()
      bowls(0).pushAll(newBowls(0))
    }
    case Refrigerate(hours) ⇒ {
      hours.foreach(dishes.show(_))
      curLine = numOperations // ends current procedure
    }
    case Serve(num) ⇒ {
      dishes.show(num)
    }
  }
}

private trait RecipeLoopCounter {
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
  // This implementation is somewhat crazy, it should be refactored
}

class BlockedLoopChefInterpreter extends ChefInterpreter {

  def execute(recipe: Recipe): Unit = {
    val loopableRecipes = recipe.recipes.mapValues { (r) ⇒
      new PartialRecipe(r) with RecipeLoopCounter with LoopablePartialRecipe
    }
    new ChefContext(loopableRecipes(recipe.mainRecipe.title), loopableRecipes).execute()
  }
}
