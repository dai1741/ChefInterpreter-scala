package jp.dai1741.parsing.chef

import scala.util.parsing.combinator._
import scala.util.Random
import scala.collection.mutable.{ ArrayBuffer, HashMap }
import scala.collection.{ TraversableOnce }
import java.util.regex.Pattern

// TODO: Support auxiliary recipe and verb loops
class ChefParsers extends JavaTokenParsers {
  
  override val whiteSpace = """[ \t　]+""".r // do not ignore \n
  
  def レシピ =
    (レシピタイトル ~ (レシピコメント.? ~> 材料表) ~ (料理時間.? ~> オーブン温度.? ~>
    作り方) ~ 配膳.? <~ 項目区切り).*
  
  def レシピタイトル = 一行の文章 <~ 項目区切り
  def レシピコメント = "^(?!・材料)".r ~>一段落 <~ 項目区切り
  
  var ingredientsParser: Parser[String] = failure("No ingredients available")
  def 材料表 = "・材料\n" ~> 材料定義.+ <~ 項目区切り ^^ { (ls) ⇒ {
    ingredientsParser =
      ("(?!)" :: ls.map(_._1).distinct.sortBy(-_.size).map(Pattern.quote)).mkString("|").r
    ls
  }}
  
  def 材料定義 = 材料名 ~ (("：" ~> 分量).? ^^ {
    _.getOrElse(new ~(None, None))
  }) <~ "\n" ^^ { case (name ~ (amount ~ iType)) ⇒
    (name → Ingredient(iType.getOrElse(dry), amount.getOrElse(-1), amount.isDefined))
  }
  
  def 材料名 = """[^：\n]+""".r ^^ { _.trim() }
  
  def 分量 = (前単位.? ~> 整数.? ~ (後単位.? ^^ {
    _.getOrElse(None)
  })) ||| "適量|少々".r ^^^ new ~(None, None)
  
  def 前単位 = "約?(大さじ|小さじ)?".r
  def 後単位 = (液状後単位 | 固形後単位 | 不明後単位) <~ "弱|強|(?:程度?)?".r
  def 液状後単位 = "(?:滴|(ミリ)?リットル|杯|cc|ml|L)".r ^^^ Some(liquid)
  def 固形後単位 =
    "(?:個|枚|(キロ)?グラム|切れ|かけ|片|玉|本|匹|束|袋|株|丁|cm|g|kg)".r ^^^ Some(dry)
  def 不明後単位 = "(?:カップ|升|合|缶)".r ^^^ None
  
  def 料理時間 = "料理時間(の目安)?：".r ~> 整数 <~ 一行の文章 <~ 項目区切り
  
  def オーブン温度 = "予めオーブンを".r ~> 整数 <~ "度に温めておいてください。" <~ 項目区切り
  
  def 作り方 = "・作り方\n" ~> (手順 <~ """。\n?|\n|\z""".r).+ <~ 項目区切り
  
  def 手順 =
    例の材料("を冷蔵庫から取り出す")                                  ^^ FromFridge |
    材料をボウル("に") <~ "入れる"                                    ^^ composite(Put) |
    材料をボウル("に") <~ "入れてゆっくり混ぜる"                      ^^ composite(Fold) |
    材料をボウル("に") <~ "加える"                                    ^^ composite(Add) |
    材料をボウル("から") <~ "取り出す"                                ^^ composite(Remove) |
    材料をボウル("に") <~ "入れて混ぜる"                              ^^ composite(Combine) |
    例の材料("を一部") ~ n番目のボウル("に") <~ "入れる"              ^^ composite(Divide) |
    "固形物を全部" ~> n番目のボウル("に") <~ "加える"                 ^^ AddDries |
    例の材料("を混ぜて液状にする")                                    ^^ LiquefyIngredient |
    n番目の略せないボウル("の中身をよく混ぜて液状にする")             ^^ LiquefyContents |
    n番目の略せないボウル("の中身を") ~ 整数 <~ "分間かき混ぜる"      ^^ composite(StirWhile) |
    例の材料("をかき混ぜながら") ~ n番目のボウル("に") <~ "入れる"    ^^ composite(StirWith) |
    n番目の略せないボウル("の中身をよく混ぜる")                       ^^ Mix |
    n番目の略せないボウル("をきれいにする")                           ^^ Clean |
    n番目の略せないボウル("の中身を") ~ n番目の <~ "オーブン皿へ移す" ^^ composite(Pour) |
    // it parses 5 rules below, but they are not implemented so far
    例の材料("を").? ~ どうする                                       ^^ composite(Verb) |
    何かし終わるまで ~> 例の材料("を").? ~ どうする                   ^^ composite(VerbUntil) |
    """[^\n。]+を添える(?=[\n。])""".r                                ^^ { (s) ⇒ 
      ServeWith(s.replaceFirst("を添える$", ""))
    } |
    "しばらく置いておく"                                              ^^^ SetAside() |
    (整数 <~ "時間").? <~ "冷蔵庫に入れておく"                        ^^ Refrigerate
  
  def composite[A, B](func: (A, B) ⇒ Operation): ((~[A, B]) ⇒ Operation) = _ match {
    case (a ~ b) ⇒ func(a, b)
  }
  
  def 配膳 = ((整数 <~ "人分|人前".r <~ 一行の文章.?) | ("さあ召し上がれ！" ^^^ 1)) ^^ Serve
  
  def 例の材料(suf: String = "") = ingredientsParser <~ suf
  // this implementaion is somewhat stupid. better to capture all string before detecting ingredient?
    
  def n番目の = ((整数 <~ "番目の").? ^^ {
    _.getOrElse(1) - 1
  })
  def n番目の略せないボウル(suf: String = "") = n番目の <~ "ボウル" <~ suf
  def n番目のボウル(particle: String) = n番目の略せないボウル(particle).? ^^ {
    _.getOrElse(0)
  }
  def 材料をボウル(particle: String) = 例の材料("を") ~ n番目のボウル(particle)
  
  // verb loop correspondence will be unchecked, so grammer validation is extremely lazy.
  def どうする = """\S+[うくぐすずつづぬふぶぷむゆる]""".r
  def 何かし終わるまで = """\S+[いきぎしじちぢにひびぴみゐり]終(?:わ|え)るまで""".r
  
  def 一行の文章 = """[^\n]+""".r
  def 一段落 = """(?:[^\n]+\n?)+""".r
  def 項目区切り = """\n+|\z""".r
  def 整数 = decimalNumber ^^ { _.toInt }
  
  case class Ingredient(var iType: IngredientType,
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
  
  // Scala's enum is pretty cool, though not useful than java's one i think
  case class IngredientType(convertData: (Int) ⇒ String)
  val dry = IngredientType(_.toString)
  val liquid = IngredientType(_.toChar.toString)
  
  class ChefStacks extends HashMap[Int, Stack[Ingredient]] {
    override def apply(i: Int) = {
      getOrElseUpdate(i, new Stack[Ingredient])
    }
    def showUntil(n: Int) {
      for (i ← 0 until n) {
        print(this(i).toSeq.reverse.map(_.toString) mkString "")
      }
      println() // should print line after each dish? or rather shouldn't print line at all?
    }
  }
  
  class Stack[A] extends ArrayBuffer[A] {
    def push(elem: A) = this += elem
    def pushAll(that: TraversableOnce[A]) = this ++= that
    
    def pop = {
      val ret = last
      trimEnd(1)
      ret
    }
    def stir(num: Int) = insert(math.max(size - num - 1, 0), pop)
  }
  
  
  
  trait Operation
  // Strategy pattern should be better, but i prefer scalability derp derp
  // class Operation(val process: () ⇒ Unit)
  
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
  
  class ChefInterpreter {
    val bowls = new ChefStacks
    val dishes = new ChefStacks
    var ingredients = new HashMap[String, Ingredient]
    
    implicit def string2ingredient(s: String): Ingredient = ingredients(s)  // this is dangerous
    
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
  
}

class VerboseChefParsers extends ChefParsers {
  override def 材料定義 = log(super.材料定義)("材料定義")
  override def 材料名 = log(super.材料名)("材料名")
  override def 手順 = log(super.手順)("手順")
}

object ChefParsers {
  def main(args: Array[String]) {
    val me = if (args.last == "-v") new VerboseChefParsers else new ChefParsers
    val res = me.parseAll(me.レシピ, io.Source.fromFile(args(0), "UTF-8").getLines mkString "\n")
    if (args.last == "-v") println(res)
  }
}
