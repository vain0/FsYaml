module YamlSpec

open NUnit.Framework
open NaturalSpec

module 整数 =
  [<Example("42", 42)>]
  [<Example("0", 0)>]
  let ``数値のみを含むYAML文字列を数値に変換できる``(yaml, expected) =
    Given yaml
    |> When (fun yaml -> let r: int = Yaml.load yaml in r)
    |> It should equal expected
    |> Verify

module 文字列 =
  [<Example("42")>]
  [<Example("hoge")>]
  let ``文字列のみを含むYAML文字列を文字列に変換できる``(yaml) =
    Given yaml
    |> When (fun yaml -> let r: string = Yaml.load yaml in r)
    |> It should equal yaml
    |> Verify

//module 独自の型 =
//  type t = Hoge | Piyo with
//    static member (?<-) (yaml: string, _Format: Yaml.Read, _: t) =
//      if yaml.ToLower() = "hoge" then Hoge else Piyo
//
//  [<Example("HoGE", true)>]
//  [<Example("piyo", false)>]
//  [<Example("foo", false)>]
//  let 独自の型に独自の変換を定義できる (yaml, isHoge) =
//    Given yaml
//    |> When (fun yaml -> let r: t = Yaml.load yaml in r)
//    |> It should equal (if isHoge then Hoge else Piyo)
//    |> Verify
