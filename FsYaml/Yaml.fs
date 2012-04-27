module Yaml

// Format class ---------------------------------------------------------------

type Read = Read with
  static member (?<-) (yaml: string, _Format: Read, _: int) = int yaml
  static member (?<-) (yaml: string, _Format: Read, _: string) = yaml
  static member (?<-) (yaml: string, _Format: Read, _: 'a) = failwith ""

type Write = Write with
  static member (?<-) (_, _Format: Write, i: int) = string i
  static member (?<-) (_, _Format: Write, s: string) = s

let inline load (yaml: string) : ^a =
  yaml ? (Read) <- Unchecked.defaultof< ^a >

let inline dump x : string =
  () ? (Write) <- x