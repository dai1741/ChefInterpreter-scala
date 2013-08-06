
import jp.dai1741.parsing.chef._

object Chef {

  def eval(script: String, parsers: ChefParsers) {
    new BlockedLoopChefInterpreter().execute(
      parsers.parseRecipe(script)
    )
  }

  def evalJapanese(script: String) = eval(script, new JapaneseChefParsers())

  def evalOriginal(script: String) = eval(script, new OriginalChefParsers())
  
  def main(args: Array[String]) {
    evalOriginal(io.Source.fromFile(args(0), "UTF-8").getLines mkString "\n")
  }

}