package jp.dai1741.parsing.chef

import scala.util.parsing.combinator._
import scala.util.Random
import scala.collection.mutable.{ ArrayBuffer, HashMap }
import scala.collection.immutable.{ TreeMap }
import scala.collection.{ TraversableOnce }

// TODO: Support auxiliary recipe and verb loops
class ChefParsers extends JavaTokenParsers {
  
  override val whiteSpace = """[ \t　]+""".r // do not ignore \n
  
  def レシピ =
    (レシピタイトル ~ (レシピコメント.? ~> 材料表) ~ (料理時間.? ~> オーブン温度.? ~>
    作り方) ~ 配膳.? <~ 項目区切り).*
  
  def レシピタイトル = 一行の文章 <~ 項目区切り
  def レシピコメント = "^(?!・材料)".r ~>一段落 <~ 項目区切り
  
  var ingredients = new TreeMap[String, Ingredient]
  def 材料表 = "・材料\n" ~> 材料定義.+ <~ 項目区切り ^^ { (ls) ⇒ {
    ingredients = TreeMap(ls: _*)
    ls
  }}
  
  def 材料定義 = 材料名 ~ ("：" ~> 分量).? <~ "\n" ^^ {
    case (name ~ Some(Some(amount) ~ iType)) ⇒ (name → Ingredient(iType, amount, true))
    case (name ~ Some(None ~ iType)) ⇒ (name → Ingredient(iType))
    case (name ~ None) ⇒ (name → defaultIngredient.copy())
  }
  
  def 材料名 = """[^：\n]+""".r ^^ { _.trim() }
  
  def 分量 = ("約".? ~> 前単位.? ~> 整数.? ~ (後単位.? ^^ {
    _.getOrElse(defaultIngredient.iType)
  })) ||| "適量|少々".r ^^^ new ~(None, defaultIngredient.iType)
  
  def 前単位 = "約|大さじ|小さじ".r
  def 後単位 = (液状後単位 | 固形後単位 | 不明後単位) <~ "弱|強|(?:程度?)?".r
  def 液状後単位 = "(?:滴|(ミリ)?リットル|杯|cc|ml|L)".r ^^^ liquid
  def 固形後単位 = "(?:個|枚|(キロ)?グラム|切れ|かけ|片|玉|本|匹|束|袋|株|丁|cm|g|kg)".r ^^^ dry
  def 不明後単位 = "(?:カップ|升|合|缶)".r ^^^ defaultIngredient.iType
  
  def 料理時間 = "料理時間(の目安)?：".r ~> 整数 <~ 一行の文章 <~ 項目区切り
  
  def オーブン温度 = "予めオーブンを".r ~> 整数 <~ "度に温めておいてください。" <~ 項目区切り
  
  def 作り方 = "・作り方\n" ~> (手順 <~ """。\n?|\n|\z""".r).+ <~ 項目区切り
  
  def 手順 =
    冷蔵庫から取り出す ^^ { FromFridge(_) } |
    ボウルに入れる ^^ { unpackOperation(_, Put) } |
    ゆっくり混ぜる ^^ { unpackOperation(_, Fold) } |
    ボウルに加える ^^ { unpackOperation(_, Add) } |
    ボウルから取り出す ^^ { unpackOperation(_, Remove) } |
    ボウルに入れて混ぜる ^^ { unpackOperation(_, Combine) } |
    一部をボウルに入れる ^^ { unpackOperation(_, Divide) } |
    固形物をボウルに加える ^^ { AddDries(_) } |
    材料を液状にする ^^ { LiquefyIngredient(_) } |
    ボウルを液状にする ^^ { LiquefyContents(_) } |
    ボウルをかき混ぜる ^^ { unpackOperation(_, StirWhile) } |
    かき混ぜながら入れる ^^ { unpackOperation(_, StirWith) } |
    ボウルをよく混ぜる ^^ { Mix(_) } |
    ボウルをきれいにする ^^ { Clean(_) } |
    オーブン皿へ移す ^^ { unpackOperation(_, Pour) } |
    // TODO: implement
    何かする ^^^ Verb("hoge", None) |
    何かし終わるまで何かする ^^^ VerbUntil(None, "hoge")|
    添える ^^^ ServeWith("hoge") |
    わきに置く ^^^ SetAside() |
    冷蔵庫に入れる ^^ { Refrigerate(_) }
  
  def unpackOperation[A, B](o: ~[A, B], func: (A, B) ⇒ Operation) = o match {
    case (a ~ b) ⇒ func(a, b)
  }
  
  def 配膳 = ((整数 <~ "人分|人前".r <~ 一行の文章.?) | ("さあ召し上がれ！" ^^^ 1)) ^^ {
    Serve(_)
  }
  
  def 冷蔵庫から取り出す = 例の材料("を冷蔵庫から取り出す")
  def ボウルに入れる = 材料をボウル("に") <~ "入れる" // Note that `材料をボウル("に入れる")` won't work, witch is optional
  def ゆっくり混ぜる = 材料をボウル("に") <~ "入れてゆっくり混ぜる"
  def ボウルに加える = 材料をボウル("に") <~ "加える"
  def ボウルから取り出す = 材料をボウル("から") <~ "取り出す"
  def ボウルに入れて混ぜる = 材料をボウル("に") <~ "入れて混ぜる"
  def 一部をボウルに入れる = 例の材料("を一部") ~ n番目のボウル("に") <~ "入れる"
  def 固形物をボウルに加える = "固形物を全部" ~> n番目のボウル("に") <~ "加える"
  def 材料を液状にする = 例の材料("を混ぜて液状にする")
  def ボウルを液状にする = n番目の略せないボウル("の中身をよく混ぜて液状にする")
  def ボウルをかき混ぜる = n番目の略せないボウル("の中身を") ~ 整数 <~ "分間かき混ぜる"
  def かき混ぜながら入れる = 例の材料("をかき混ぜながら") ~ n番目のボウル("に") <~ "入れる"
  def ボウルをよく混ぜる = n番目の略せないボウル("の中身をよく混ぜる")
  def ボウルをきれいにする = n番目の略せないボウル("をきれいにする")
  def オーブン皿へ移す = n番目の略せないボウル("の中身を") ~ (n番目の <~ "オーブン皿へ移す")
  
  // it parses 5 rules below, but they are not implemented so far
  def 何かする = 例の材料("を").? <~ どうする
  def 何かし終わるまで何かする = 何かし終わるまで ~> 例の材料("を").? <~ どうする
  def 添える = """[^\n]+を添える(?=[\n。])""".r ^^ { _.replaceFirst("を添える$", "") }
  def わきに置く = "しばらく置いておく"
  def 冷蔵庫に入れる = (整数 <~ "時間").? <~ "冷蔵庫に入れておく"
  
  def 例の材料(suf: String = "") =
    // TODO: Stop build regex rule each time for efficiency
    // Or rather, this implementaion is stupid. better to capture all string before detecting ingredient?
    (Parser { ((ingredients.keys.toSeq.reverse mkString "|").r)(_) }) <~ suf
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
  
  case class Ingredient(var iType: IngredientType = dry,
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
  val defaultIngredient = Ingredient()
  
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
  val bowls = new ChefStacks
  val dishes = new ChefStacks
  
  
  
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
  case class Verb(verb: String, ingredient: Option[String]) extends Operation
  case class VerbUntil(ingredient: Option[String], verbed: String) extends Operation
  case class SetAside() extends Operation
  case class ServeWith(recipe: String) extends Operation
  case class Refrigerate(hours: Option[Int]) extends Operation
  
  case class Serve(num: Int) extends Operation
  
  class ChefInterpreter {
    val bowls = new ChefStacks
    val dishes = new ChefStacks
    var ingredients = new TreeMap[String, Ingredient]
    
    implicit def string2ingredient(s: String): Ingredient = ingredients(s)  // this is dangerous
    
    def process(expr: Operation) = expr match {
      case FromFridge(ingred) ⇒ {
        ingredients(ingred).data = readInt // probably Scanner#nextInt is better
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
  override def 材料表 = super.材料表 ^^ { (x) ⇒
    println("ingredients: " + ingredients)
    x
  }
  override def 材料定義 = log(super.材料定義)("材料定義")
  override def 材料名 = log(super.材料名)("材料名")
  override def 手順 = log(super.手順)("手順") ^^ { (x) ⇒
    println(bowls(0).toSeq.reverse.map(_.toString) mkString "")
    x
  }
}

object ChefParsers {
  def main(args: Array[String]) {
    val me = if (args.last == "-v") new VerboseChefParsers else new ChefParsers
    val res = me.parseAll(me.レシピ, io.Source.fromFile(args(0), "UTF-8").getLines mkString "\n")
    if (args.last == "-v") println(res)
  }
}
