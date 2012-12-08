
import jp.dai1741.parsing.chef._

object Chef {

  def evalJapanese(script: String) {
    new BlockedLoopChefInterpreter().execute(
      new JapaneseChefParsers().parseRecipe(script)
    )
  }
  
  def main(args: Array[String]) {
    evalJapanese(io.Source.fromFile(args(0), "UTF-8").getLines mkString "\n")
  }

}