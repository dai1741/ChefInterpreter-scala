package jp.dai1741.parsing.chef

import scala.util.parsing.combinator._
import java.util.regex.Pattern

trait ChefParsers {
  def parseRecipe(recipe: String): Recipe
}

// TODO: rename parsers
class OriginalChefParsers extends ChefParsers with JavaTokenParsers {
  
  override val whiteSpace = """[ \t　]+""".r  // do not ignore \n
  
  def parseRecipe(recipeStr: String) = {
    parseAll(レシピ, recipeStr) match {
      case Success(ls @ (_::_), in) ⇒ {
        val recipes = ls.map {
          case title ~ Pair(ingreds, operations) ~ service ⇒ new PartialRecipe(
            title, ingreds, operations ++ service
          )
        }
        new Recipe(recipes.head, Map(recipes.map { (r) ⇒ (r.title -> r) }:_*))
      }
      case Success(Nil, in) ⇒ throw new IllegalRecipeException(
        "Parse succeeded, but no recipe exists")
      case NoSuccess(msg, in) ⇒ throw new IllegalRecipeException("Parse failed: " + msg)
    }
  }

  // def recipe = 
  //   (recipeTitle ~ (comments.? ~> ingredientList <~ cookingTime.? <~ ovenTemperature.? >>
  //   method) ~ serves.? <~ separators).*


  
  def レシピ =
    (レシピタイトル ~ (レシピコメント.? ~> 材料表 <~ 料理時間.? <~ オーブン温度.? >>
    作り方) ~ 配膳.? <~ 項目区切り).*
  
  def レシピタイトル = """(?:\.?[^\n.]+)+""".r <~ "." <~ 項目区切り ^^ { _.toLowerCase }
  def レシピコメント = """^(?!Ingredients\.)""".r ~> 一段落 <~ 項目区切り
  
  def 材料表 = "Ingredients.\n" ~> 材料定義.+ <~ 項目区切り
  
  def 材料定義 = (分量.? ^^ {
    _.getOrElse(new ~(None, None))
  }) ~ 材料名 <~ "\n" ^^ { case ((amount ~ iType) ~ name) ⇒
    Ingredient(name, iType.getOrElse(IngredientType.dry), amount.getOrElse(-1), amount.isDefined)
  }
  
  def 材料名 = 一行の文章 ^^ { _.trim() }
  
  def 分量 = (整数.? ~ (後単位.? ^^ {
    _.flatMap(identity)
  }))
  
  def 後単位 = "heaped|(level)?".r ~> (液状後単位 | 固形後単位 | 不明後単位)
  def 液状後単位 = "(?:ml|l|dash(es)?)".r ^^^ Some(IngredientType.liquid)
  def 固形後単位 = "(?:g|kg|pinch(es)?)".r ^^^ Some(IngredientType.dry)
  def 不明後単位 = "(?:cups?|teaspoons?|tablespoons?)".r ^^^ None
  
  def 料理時間 = "Cooking time:".r ~> 整数 <~ """(hours?|minutes?)\.""".r <~ 項目区切り
  
  def オーブン温度 = "Pre-heat oven to temperature ".r ~> 整数 <~ " degrees Celsius" <~ (" (gas mark " <~ 整数 <~ ")").? <~ "." <~ 項目区切り
  
  def 作り方(ingreds: List[Ingredient]) = {
    val 材料名 = 
      ("(?!)" :: ingreds.map(_.name).distinct.sortBy(-_.size).map(Pattern.quote)).mkString("|").r
    
    "Method.\n" ~> (手順(材料名) <~ """\.\n?|\n|\z""".r).+ <~ 項目区切り ^^ { (ingreds, _) }
  }
  
  def 手順(ingreds: Parser[String]) = {
  
    def composite[A, B](func: (A, B) ⇒ Operation): ((~[A, B]) ⇒ Operation) = _ match {
      case (a ~ b) ⇒ func(a, b)
    }
    def ingred(suf: String = "") = ("the".? ~> ingreds) <~ suf
    
    "Take" ~> ingred("from refrigerator")               ^^ FromFridge |
    "Put" ~> (ingred() ~ intoNthBowl)                   ^^ composite(Put) |
    "Fold" ~> ingred() ~ intoNthBowl                    ^^ composite(Fold) |
    "Add" ~> ingred() ~ implicitNthBowl("to")           ^^ composite(Add) |
    "Remove" ~> ingred() ~ implicitNthBowl("from")      ^^ composite(Remove) |
    "Combine" ~> ingred() ~ implicitNthBowl("into")     ^^ composite(Combine) |
    "Divide" ~> ingred() ~ implicitNthBowl("into")      ^^ composite(Divide) |
    "Add dry ingredients" ~> implicitNthBowl("to")      ^^ AddDries |
    liquefy ~> ingred()                                 ^^ LiquefyIngredient |
    liquefy ~> "contents of the" ~> nthBowl             ^^ LiquefyContents |
    "Stir" ~> implicitNthBowl() ~ ("for" ~> 整数 <~ "minutes") ^^ composite(StirWhile) |
    "Stir" ~> ingred() ~ ("into" ~> nthBowl)            ^^ composite(StirWith) |
    "Stir" ~> implicitNthBowl() <~ "well"               ^^ Mix |
    "Clean" ~> nthBowl                                  ^^ Clean |
    "Pour contents of" ~> nthBowl ~ ("into" ~> "the".? ~> nth <~ "baking dish") ^^ composite(Pour) |
    verb ~ ingred()                                     ^^ { case (v ~ i) ⇒
      Verb(i, v)
    } |||
    verb ~> ingred().? ~ ("until" ~> verbed)            ^^ composite(VerbUntil) |||
    "Serve with " ~> """[^.]+""".r                      ^^ { (s) ⇒
      ServeWith(s.toLowerCase)  // watch out, these are case-insensitive
    } |
    "Set aside"                                         ^^^ SetAside() |
    "Refrigerate" ~> ("for " ~> 整数 <~ "hours").?      ^^ Refrigerate
  }

  def nth = (整数 <~ "st|nd|th".r).? ^^ {
    _.getOrElse(1) - 1
  }
  def nthBowl = "the".? ~> nth <~ "mixing bowl"
  def implicitNthBowl(pre: String = "") = (pre ~> nthBowl).? ^^ {
    _.getOrElse(0)
  }
  def intoNthBowl = "into" ~> nthBowl
  def liquefy = """Liqu[ei]fy""".r
  
  // verb loop correspondence will be unchecked, so grammer validation is extremely lazy.
  def verb = """\w+""".r
  def verbed = """\w+""".r
  
  def 配膳 = ("Serves" ~> 整数 <~ ".") ^^ Serve
  
  def 一行の文章 = """[^\n]+""".r
  def 一段落 = """(?:[^\n]+\n?)+""".r
  def 項目区切り = """\n+|\z""".r
  def 整数 = """\d+""".r ^^ { _.toInt }  // number???
}


class JapaneseChefParsers extends ChefParsers with JavaTokenParsers {
  
  override val whiteSpace = """[ \t　]+""".r  // do not ignore \n
  
  def parseRecipe(recipe: String) = {
    parseAll(レシピ, recipe) match {
      case Success(ls @ (_::_), in) ⇒ {
        val recipes = ls.map {
          case title ~ Pair(ingreds, operations) ~ service ⇒ new PartialRecipe(
            title, ingreds, operations ++ service
          )
        }
        new Recipe(recipes.head, Map(recipes.map { (r) ⇒ (r.title -> r) }:_*))
      }
      case Success(Nil, in) ⇒ throw new IllegalRecipeException(
        "Parse succeeded, but no recipe exists")
      case NoSuccess(msg, in) ⇒ throw new IllegalRecipeException("Parse failed: " + msg)
    }
  }
  
  def レシピ =
    (レシピタイトル ~ (レシピコメント.? ~> 材料表 <~ 料理時間.? <~ オーブン温度.? >>
    作り方) ~ 配膳.? <~ 項目区切り).*
  
  def レシピタイトル = 一行の文章 <~ 項目区切り
  def レシピコメント = "^(?!・材料)".r ~> 一段落 <~ 項目区切り
  
  def 材料表 = "・材料\n" ~> 材料定義.+ <~ 項目区切り
  
  def 材料定義 = 材料名 ~ (("：" ~> 分量).? ^^ {
    _.getOrElse(new ~(None, None))
  }) <~ "\n" ^^ { case (name ~ (amount ~ iType)) ⇒
    Ingredient(name, iType.getOrElse(IngredientType.dry), amount.getOrElse(-1), amount.isDefined)
  }
  
  def 材料名 = """[^：\n]+""".r ^^ { _.trim() }
  
  def 分量 = (前単位.? ~> 整数.? ~ (後単位.? ^^ {
    _.flatMap(identity)
  })) ||| "適量|少々".r ^^^ new ~(None, None)
  
  def 前単位 = "約?(大さじ|小さじ)?".r
  def 後単位 = (液状後単位 | 固形後単位 | 不明後単位) <~ "弱|強|(?:程度?)?".r
  def 液状後単位 = "(?:滴|(ミリ)?リットル|杯|cc|ml|L)".r ^^^ Some(IngredientType.liquid)
  def 固形後単位 =
    "(?:個|枚|(キロ)?グラム|切れ|かけ|片|玉|本|匹|束|袋|株|丁|cm|g|kg)".r ^^^
      Some(IngredientType.dry)
  def 不明後単位 = "(?:カップ|升|合|缶)".r ^^^ None
  
  def 料理時間 = "料理時間(の目安)?：".r ~> 整数 <~ 一行の文章 <~ 項目区切り
  
  def オーブン温度 = "予めオーブンを".r ~> 整数 <~ "度に温めておいてください。" <~ 項目区切り
  
  def 作り方(ingreds: List[Ingredient]) = {
    val 材料名 = 
      ("(?!)" :: ingreds.map(_.name).distinct.sortBy(-_.size).map(Pattern.quote)).mkString("|").r
    
    "・作り方\n" ~> (手順(材料名) <~ """。\n?|\n|\z""".r).+ <~ 項目区切り ^^ { (ingreds, _) }
  }
  
  def 手順(材料名: Parser[String]) = {
  
    def composite[A, B](func: (A, B) ⇒ Operation): ((~[A, B]) ⇒ Operation) = _ match {
      case (a ~ b) ⇒ func(a, b)
    }
    def 材料(suf: String) = 材料名 <~ suf
    def 材料を略せるボウル(particle: String) = 材料("を") ~ 略せるボウル(particle)
    
    材料("を冷蔵庫から取り出す")                               ^^ FromFridge |
    材料を略せるボウル("に") <~ "入れる"                       ^^ composite(Put) |
    材料を略せるボウル("に") <~ "入れてゆっくり混ぜる"         ^^ composite(Fold) |
    材料を略せるボウル("に") <~ "加える"                       ^^ composite(Add) |
    材料を略せるボウル("から") <~ "取り出す"                   ^^ composite(Remove) |
    材料を略せるボウル("に") <~ "入れて混ぜる"                 ^^ composite(Combine) |
    材料("を一部") ~ 略せるボウル("に") <~ "入れる"            ^^ composite(Divide) |
    "固形物を全部" ~> 略せるボウル("に") <~ "加える"           ^^ AddDries |
    材料("を混ぜて液状にする")                                 ^^ LiquefyIngredient |
    ボウル("の中身をよく混ぜて液状にする")                     ^^ LiquefyContents |
    ボウル("の中身を") ~ 整数 <~ "分間かき混ぜる"              ^^ composite(StirWhile) |
    材料("をかき混ぜながら") ~ 略せるボウル("に") <~ "入れる"  ^^ composite(StirWith) |
    ボウル("の中身をよく混ぜる")                               ^^ Mix |
    ボウル("をきれいにする")                                   ^^ Clean |
    ボウル("の中身を") ~ n番目の <~ "オーブン皿へ移す"         ^^ composite(Pour) |
    材料("を") ~ どうする                                      ^^ composite(Verb) |
    何かし終わるまで ~> 材料("を").? ~ どうする                ^^ composite(VerbUntil) |
    """[^\n。]+を添える(?=[\n。])""".r                         ^^ { (s) ⇒ 
      ServeWith(s.replaceFirst("を添える$", ""))
    } |
    "しばらく置いておく"                                       ^^^ SetAside() |
    (整数 <~ "時間").? <~ "冷蔵庫に入れておく"                 ^^ Refrigerate
  }
  
  // verb loop correspondence will be unchecked, so grammer validation is extremely lazy.
  def どうする = """\S+[うくぐすずつづぬふぶぷむゆる]""".r
  def 何かし終わるまで =
    """\S+[いきぎしじちぢにひびぴみゐり|えけげせぜてでねへべぺめゑれ]終(?:わ|え)るまで""".r
  
  def 配膳 = ((整数 <~ "人分|人前".r <~ 一行の文章.?) | ("さあ召し上がれ！" ^^^ 1)) ^^ Serve
    
  def n番目の = ((整数 <~ "番目の").? ^^ {
    _.getOrElse(1) - 1
  })
  def ボウル(suf: String = "") = n番目の <~ "ボウル" <~ suf
  def 略せるボウル(particle: String) = ボウル(particle).? ^^ {
    _.getOrElse(0)
  }
  
  def 一行の文章 = """[^\n]+""".r
  def 一段落 = """(?:[^\n]+\n?)+""".r
  def 項目区切り = """\n+|\z""".r
  def 整数 = decimalNumber ^^ { _.toInt }
  
}

class VerboseJapaneseChefParsers extends JapaneseChefParsers {
  override def 材料定義 = log(super.材料定義)("材料定義")
  override def 材料名 = log(super.材料名)("材料名")
  override def 手順(材料名: Parser[String]) = log(super.手順(材料名))("手順")
}

object ChefParsers {
  def main(args: Array[String]) {
    val me = if (args.last == "-v") new VerboseJapaneseChefParsers else new JapaneseChefParsers()
    val res = me.parseRecipe(io.Source.fromFile(args(0), "UTF-8").getLines mkString "\n")
    if (args.last == "-v") println(res)
  }
}
