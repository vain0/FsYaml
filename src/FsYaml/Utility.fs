module internal FsYaml.Utility

open System.Collections.Generic

module Resources =
  type private Dummy = Dummy
  let private manager = System.Resources.ResourceManager("Resources", typeof<Dummy>.Assembly)

  let getString key = manager.GetString(key)

module String =
  open System

  let toLower (x: string) = x.ToLower()

module Type =
  open System
  open Microsoft.FSharp.Reflection

  let typeAlias = [
    (typeof<obj>, "obj")
    (typeof<exn>, "exn")
    (typeof<string>, "string")
    (typeof<float32>, "float32")
    (typeof<float>, "float")
    (typeof<sbyte>, "sbyte")
    (typeof<byte>, "byte")
    (typeof<int16>, "int16")
    (typeof<uint16>, "uint16")
    (typeof<int>, "int")
    (typeof<uint32>, "uint32")
    (typeof<int64>, "int64")
    (typeof<uint64>, "uint64")
    (typeof<char>, "char")
    (typeof<bool>, "bool")
    (typeof<decimal>, "decimal")
    (typeof<bigint>, "bigint")
    (typedefof<list<_>>, "list")
    (typedefof<seq<_>>, "seq")
    (typedefof<option<_>>, "option")
    (typedefof<ResizeArray<_>>, "ResizeArray")
    (typedefof<Map<_, _>>, "Map")
  ]

  let specialGenericType = [
    typedefof<list<_>>
    typedefof<option<_>>
    typedefof<seq<_>>
  ]

  let typeName (t: Type) =
    let t = if t.IsGenericType then t.GetGenericTypeDefinition() else t
    match List.tryFind (fst >> (=) t) typeAlias with
    | Some (_, alias) -> alias
    | None ->
      let name = t.Name
      match name.LastIndexOf("`") with
      | -1 -> name
      | x -> name.Substring(0, x)
      

  let rec print (t: Type) =
    if t.IsArray then printArray t
    elif FSharpType.IsTuple(t) then printTuple t
    elif t.IsGenericType then printGeneric t
    else typeName t
  and internal printTuple (t: Type) =
    FSharpType.GetTupleElements(t)
    |> Array.map (fun elemType ->
      let elemName = print elemType
      if FSharpType.IsTuple(elemType) then
        sprintf "(%s)" elemName
      else
        elemName
    )
    |> String.concat " * "
  and internal printGeneric (t: Type) =
    match List.tryFind ((=) (t.GetGenericTypeDefinition())) specialGenericType with
    | Some (_) ->
      let elem = t.GetGenericArguments().[0]
      let elemName = print elem
      let name = typeName t
      if elem.IsGenericType then
        sprintf "(%s) %s" elemName name
      else
        sprintf "%s %s" elemName name
    | None ->
      let args = t.GetGenericArguments() |> Array.map print |> String.concat ", "
      sprintf "%s<%s>" (typeName t) args
  and internal printArray (t: Type) =
    let elem = t.GetElementType()
    sprintf "%s[]" (print elem)

module Attribute =
  open System
  open System.Reflection

  let tryGetCustomAttribute<'a when 'a :> Attribute> (x: MemberInfo) =
    let attr = x.GetCustomAttribute(typeof<'a>, false)
    if attr = null then
      None
    else
      Some (attr :?> 'a)
      
module PropertyInfo =
  open System.Reflection

  let print (x: PropertyInfo) = sprintf "%s.%s" (Type.print x.DeclaringType) x.Name

module Union =
  open Microsoft.FSharp.Reflection

  let printCase (x: UnionCaseInfo) = sprintf "%s.%s" (Type.print x.DeclaringType) x.Name

module Seq =
  let tryZip xs ys =
    if Seq.length xs <> Seq.length ys then
      None
    else
      Some (Seq.zip xs ys)

module Option =
  let filter f = Option.bind (fun x -> if f x then Some x else None)

  let getOrElse f =
    function
    | Some x -> x
    | None -> f ()

/// Represents an ordered map.
type ArrayMap<'k, 'v> = array<KeyValuePair<'k, 'v>>

module ArrayMap =
  open System.Collections.Generic

  let ofSeq (kvs: seq<'k * 'v>): ArrayMap<'k, 'v> =
    kvs |> Seq.map KeyValuePair |> Seq.toArray

  let toSeq (kvs: ArrayMap<'k, 'v>): seq<'k * 'v> =
    kvs |> Array.map (|KeyValue|) :> seq<_>

  let ofList (kvs: list<'k * 'v>) = kvs |> ofSeq
  let toList (kvs: ArrayMap<'k, 'v>) = kvs |> List.ofArray

  let ofArray (kvs: array<KeyValuePair<'k, 'v>>): ArrayMap<'k, 'v> = kvs
  let toArray (kvs: ArrayMap<'k, 'v>): array<KeyValuePair<'k, 'v>> = kvs

  let empty<'k, 'v> : ArrayMap<'k, 'v> =
    Array.empty<KeyValuePair<'k, 'v>>

  let singleton (key: 'k) (value: 'v): ArrayMap<'k, 'v> =
    [|KeyValuePair(key, value)|]

  let tryPick f (kvs: ArrayMap<'k, 'v>) =
    kvs |> Seq.tryPick (fun (KeyValue (k, v)) -> f k v)

  let pick f kvs =
    kvs |> tryPick f |> Option.getOrElse (fun () -> raise (KeyNotFoundException()))

let fsharpAsembly = typedefof<list<_>>.Assembly

module ObjectElementSeq =
  open System
  open System.Collections.Generic
  open System.Linq
  open Microsoft.FSharp.Reflection

  let cast (t: Type) (xs: obj seq) =
    let enumerable = typeof<Enumerable>
    let cast =
      let nonGeneric = enumerable.GetMethod("Cast")
      nonGeneric.MakeGenericMethod([| t |])
    cast.Invoke(null, [| xs |])

  let toList (t: Type) (xs: obj seq) =
    let listType = typedefof<list<_>>.MakeGenericType(t)
    let empty = listType.GetProperty("Empty").GetValue(null)
    let consMethod = listType.GetMethod("Cons")
    let cons xs x = consMethod.Invoke(null, [| x; xs |])

    Enumerable.Reverse(xs) |> Seq.fold cons empty

  let toSet (t: Type) (xs: obj seq) =
    let setType = typedefof<Set<_>>.MakeGenericType(t)
    let parameter = xs |> cast t
    let parameterType = typedefof<seq<_>>.MakeGenericType(t)
    let constructor' = setType.GetConstructor([| parameterType |])
    constructor'.Invoke([| parameter |])
    
  let toMap (keyType: Type) (valueType: Type) (xs: (obj * obj) seq) =
    let tupleType = typedefof<_ * _>.MakeGenericType([| keyType; valueType |])
    let parameter = xs |> Seq.map (fun (k, v) -> FSharpValue.MakeTuple([| k; v |], tupleType)) |> cast tupleType
    let parameterType = typedefof<seq<_>>.MakeGenericType([| tupleType |])
    let mapType = typedefof<Map<_, _>>.MakeGenericType([| keyType; valueType |])
    let constructor' = mapType.GetConstructor([| parameterType |])
    constructor'.Invoke([| parameter |])

  let toArrayMap (keyType: Type) (valueType: Type) (xs: (obj * obj) seq) =
    let pairType = typedefof<KeyValuePair<_, _>>.MakeGenericType([| keyType; valueType |])
    let pairConstructor = pairType.GetConstructor([| keyType; valueType |])
    let xs = xs |> Seq.toArray
    let array = Array.CreateInstance(pairType, xs.Length)
    xs |> Array.iteri (fun i (k, v) ->
      let kv = pairConstructor.Invoke([| k; v |])
      array.SetValue(kv, i)
    )
    array :> obj

  let toArray (t: Type) (xs: obj seq) =
    let array = Array.CreateInstance(t, xs.Count())
    xs |> Seq.iteri (fun i x -> array.SetValue(x, i))
    box array

module RuntimeSeq =
  open System
  open Microsoft.FSharp.Reflection

  let seqModule = fsharpAsembly.GetType("Microsoft.FSharp.Collections.SeqModule")

  let elementType (t: Type) =
    if t.IsArray then
      t.GetElementType()
    else
      t.GetGenericArguments().[0]

  let map (f: obj -> 'a) (t: Type) (xs: obj): 'a seq =
    let elementType = elementType t
    let mapping =
      let mappingFunctionType = typedefof<_ -> _>.MakeGenericType([| elementType; typeof<'a> |])
      FSharpValue.MakeFunction(mappingFunctionType, fun x -> f x :> obj)
    let mapFunc = seqModule.GetMethod("Map").MakeGenericMethod(elementType, typeof<'a>)
    mapFunc.Invoke(null, [| mapping; xs |]) :?> seq<'a>

module RuntimeArrayMap =
  open System
  open Microsoft.FSharp.Reflection

  /// Gets the key/value types from the specified type
  /// which represents typeof<ArrayMap<_, _>>.
  let elementTypes (t: Type) =
    let pairType = t.GetElementType()
    let types = pairType.GetGenericArguments()
    (types.[0], types.[1])

  let toSeq (typ: Type) (arrayMap: obj): (obj * obj) seq =
    let pairType = typ.GetElementType()
    let keyProperty = pairType.GetProperty("Key")
    let valueProperty = pairType.GetProperty("Value")
    let tupleFromPair kv = (keyProperty.GetValue(kv), valueProperty.GetValue(kv))
    arrayMap |> RuntimeSeq.map tupleFromPair (arrayMap.GetType())

module RuntimeMap =
  open System
  open Microsoft.FSharp.Reflection

  let mapModule = fsharpAsembly.GetType("Microsoft.FSharp.Collections.MapModule")

  let elementTypes (t: Type) = let ts = t.GetGenericArguments() in (ts.[0], ts.[1])

  let toSeq (t: Type) (map: obj): (obj * obj) seq =
    let keyType, valueType = elementTypes t
    let toListFunc = mapModule.GetMethod("ToSeq").MakeGenericMethod(keyType, valueType)
    let resultSeq = toListFunc.Invoke(null, [| map |])
    let resultSeqType = resultSeq.GetType()
    RuntimeSeq.map (fun kv -> let elems = FSharpValue.GetTupleFields(kv) in (elems.[0], elems.[1])) resultSeqType resultSeq
