import org.specs2.mutable._
import jp.dai1741.parsing.chef._
import scala.io.Source

object JapaneseChefParsersSpec extends Specification {
  sequential
  
  val parserJp = new JapaneseChefParsers()
  def getResouceString(name: String) =
      Source.fromURL(getClass.getResource(name))("UTF-8").getLines mkString "\n"
  
  lazy val helloRecipe = parserJp.parseRecipe(getResouceString("/chef_hello_jp.txt"))
  lazy val fibRecipe = parserJp.parseRecipe(getResouceString("/chef_fib_jp.txt"))
  lazy val invalidRecipe = parserJp.parseRecipe(getResouceString("/chef_invalid_jp.txt"))
  
  "Japanese Chef parsers" should {
    "parse 'chef_hello_jp.txt'" in {
      helloRecipe must not be null
    }
    "parse 'chef_fib_jp.txt'" in {
      fibRecipe must not be null
    }
    "not parse 'chef_invalid_jp.txt'" in {
      invalidRecipe must throwAn[IllegalRecipeException]
    }
  }
  
  "Recipe" should {
    val recipes = List(helloRecipe, fibRecipe)
    
    "have at least one sub recipe" in {
      recipes.foreach { (r) =>
        r.recipes must not be empty
      }
    }
    "have at least one operation" in {
      recipes.foreach { (r) =>
        r.mainRecipe.operations must not be empty
      }
    }
  }
  
  "Parse result of 'chef_hello_jp.txt'" should {
    val mainRecipe = helloRecipe.mainRecipe
  
    "have a main recipe with a title 'ハロー・ワールド・スフレ'" in {
      mainRecipe.title must be equalTo "ハロー・ワールド・スフレ"
    }
    "have 9 ingredients in the main recipe" in {
      mainRecipe.ingreds must have size 9
    }
    "have a ingredient named 'ズッキーニ' in the main recipe" in {
      mainRecipe.ingreds must haveOneElementLike { case i => i.name must be equalTo "ズッキーニ" }
    }
    "have 15 operations in the main recipe" in {
      // note that operations are methods PLUS optional serve
      mainRecipe.operations must have size 15
    }
    "have no auxiliary recipes" in {
      helloRecipe.recipes must havePair(mainRecipe.title → mainRecipe)
      helloRecipe.recipes must have size 1
    }
  }
  
  "Parse result of 'chef_fib_jp.txt'" should {
  
    "have a auxiliary recipe with a title 'カラメルソース'" in {
      fibRecipe.recipes must haveKey("カラメルソース")
      fibRecipe.recipes must not have havePair("カラメルソース" → fibRecipe.mainRecipe)
    }
    "have 2 refrigerate operations in the auxiliary recipe" in {
      fibRecipe.recipes("カラメルソース").operations.collect {
        case o: Refrigerate ⇒ o
      } must have size 2
    }
    "have a auxiliary recipe which invokes itself" in {
      fibRecipe.recipes("カラメルソース").operations must haveOneElementLike {
        case ServeWith(recipe) ⇒ recipe must be equalTo "カラメルソース"
      }
    }
    "have 250g butter in the main recipe" in {
      fibRecipe.mainRecipe.ingreds must haveOneElementLike {
        case i if i.name == "バター" ⇒ {
          i.data must be equalTo 250
          i.iType must be equalTo IngredientType.dry
        }
      }
    }
  }

}