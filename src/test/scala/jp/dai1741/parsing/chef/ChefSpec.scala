import org.specs2.mutable._
import jp.dai1741.parsing.chef._
import scala.io.Source
import java.io.{ ByteArrayOutputStream, StringReader }

object ChefSpec extends Specification {
  
  def getResouceString(name: String) =
    Source.fromURL(getClass.getResource(name))("UTF-8").getLines mkString "\n"
  
  def getStdOutIn(st: ⇒ Any) = {
    val stream = new ByteArrayOutputStream
    Console.withOut(stream) { st }
    stream.toString
  }
  
  def checkRecipe(filePath: String, expected: String) {
    getStdOutIn {
      Chef.evalJapanese(getResouceString(filePath))
    }.replaceAll("""(?<=^|\n) | (?=$|\n)""", "") must be matching expected.r
  }
  
  "Chef runtime" should {
  
    "output 'Hello world!' when evaluating 'chef_hello_jp.txt'" in {
      checkRecipe("/chef_hello_jp.txt", "Hello world!")
    }
    
    "output broken fibonacci numbers when evaluating 'chef_fib_jp.txt'" in {
      checkRecipe("/chef_fib_jp.txt", "1 0 2 1 3 1 4 1 5 1 6 1 7 1 8 1 9 1 10 1 11 1 12 1 13 1 14 1 15 1 16 1 17 1 18 1 19 1 20 1 21 1 22 1 23 1 24 1 25 1 26 1 27 1 28 1 29 1 30 1 31 1 32 1 33 1 34 1 35 1 36 1 37 1 38 1 39 1 40 1 41 1 42 1 43 1 44 1 45 1 46 1 47 1 48 1 49 1 50 1 51 1 52 1 53 1 54 1 55 1 56 1 57 1 58 1 59 1 60 1 61 1 62 1 63 1 64 1 65 1 66 1 67 1 68 1 69 1 70 1 71 1 72 1 73 1 74 1 75 1 76 1 77 1 78 1 79 1 80 1 81 1 82 1 83 1 84 1 85 1 86 1 87 1 88 1 89 1 90 1 91 1 92 1 93 1 94 1 95 1 96 1 97 1 98 1 99 1 1")
    }
    
    "output correct data for some samples" in {
      List(
        ("/sample_auxiliary.txt", "10 10 10"),
        ("/sample_operations.txt", "3 891 1180"),
        ("/sample_loops.txt", "996"),
        ("/sample_count10.txt", """1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n"""),
        ("/sample_instruments.txt", """([1259d ]+\n){2}1 55 1 1\n222""")
      ).foreach { case (recipe, expected) ⇒
        checkRecipe(recipe, expected)
      }
      
      Console.withIn(new StringReader("15\n230\n")) {
        checkRecipe("/sample_io.txt", "245")
      }
    }
  }

}