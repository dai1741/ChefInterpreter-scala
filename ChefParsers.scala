package jp.dai1741.parsing.chef

import scala.util.parsing.combinator._

import java.util.regex.Pattern
import jp.dai1741.parsing.chef.ChefOperations._
import jp.dai1741.parsing.chef.ChefProps._

class ChefParsers extends JavaTokenParsers {
  
  override val whiteSpace = """[ \t　]+""".r // do not ignore \n
  
  def レシピ =
    (レシピタイトル ~ (レシピコメント.? ~> 材料表 <~ 料理時間.? <~ オーブン温度.? >>
    作り方) ~ 配膳.? <~ 項目区切り).*
  
  def レシピタイトル = 一行の文章 <~ 項目区切り
  def レシピコメント = "^(?!・材料)".r ~>一段落 <~ 項目区切り
  
  def 材料表 = "・材料\n" ~> 材料定義.+ <~ 項目区切り
  
  def 材料定義 = 材料名 ~ (("：" ~> 分量).? ^^ {
    _.getOrElse(new ~(None, None))
  }) <~ "\n" ^^ { case (name ~ (amount ~ iType)) ⇒
    (name → Ingredient(iType.getOrElse(dry), amount.getOrElse(-1), amount.isDefined))
  }
  
  def 材料名 = """[^：\n]+""".r ^^ { _.trim() }
  
  def 分量 = (前単位.? ~> 整数.? ~ (後単位.? ^^ {
    _.flatMap(identity)
  })) ||| "適量|少々".r ^^^ new ~(None, None)
  
  def 前単位 = "約?(大さじ|小さじ)?".r
  def 後単位 = (液状後単位 | 固形後単位 | 不明後単位) <~ "弱|強|(?:程度?)?".r
  def 液状後単位 = "(?:滴|(ミリ)?リットル|杯|cc|ml|L)".r ^^^ Some(liquid)
  def 固形後単位 =
    "(?:個|枚|(キロ)?グラム|切れ|かけ|片|玉|本|匹|束|袋|株|丁|cm|g|kg)".r ^^^ Some(dry)
  def 不明後単位 = "(?:カップ|升|合|缶)".r ^^^ None
  
  def 料理時間 = "料理時間(の目安)?：".r ~> 整数 <~ 一行の文章 <~ 項目区切り
  
  def オーブン温度 = "予めオーブンを".r ~> 整数 <~ "度に温めておいてください。" <~ 項目区切り
  
  def 作り方(ingreds: List[(String, Ingredient)]) = {
    val 材料名 = 
      ("(?!)" :: ingreds.map(_._1).distinct.sortBy(-_.size).map(Pattern.quote)).mkString("|").r
    
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
    何かし終わるまで ~> 材料("を").? ~ どうする                ^^ composite(VerbUntil) |
    """[^\n。]+を添える(?=[\n。])""".r                         ^^ { (s) ⇒ 
      ServeWith(s.replaceFirst("を添える$", ""))
    } |
    "しばらく置いておく"                                       ^^^ SetAside() |
    (整数 <~ "時間").? <~ "冷蔵庫に入れておく"                 ^^ Refrigerate |
    材料("を").? ~ どうする                                    ^^ composite(Verb)
  }
  
  // verb loop correspondence will be unchecked, so grammer validation is extremely lazy.
  def どうする = """\S+[うくぐすずつづぬふぶぷむゆる]""".r
  def 何かし終わるまで = """\S+[いきぎしじちぢにひびぴみゐり]終(?:わ|え)るまで""".r
  
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

class VerboseChefParsers extends ChefParsers {
  override def 材料定義 = log(super.材料定義)("材料定義")
  override def 材料名 = log(super.材料名)("材料名")
  override def 手順(材料名: Parser[String]) = log(super.手順(材料名))("手順")
}

object ChefParsers {
  def main(args: Array[String]) {
    val me = if (args.last == "-v") new VerboseChefParsers else new ChefParsers
    val res = me.parseAll(me.レシピ, io.Source.fromFile(args(0), "UTF-8").getLines mkString "\n")
    if (args.last == "-v") println(res)
  }
}
