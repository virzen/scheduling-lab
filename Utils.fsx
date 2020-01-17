module Utils

open System

let joinWith (separator: string) (iterable: seq<string>) = String.Join(separator, iterable)

let splitOn (separator: string) (s: string): string[] =
  s.Split(separator)

let padLeft (char: char) (length: int) (s:string): string =
  s.PadLeft(length, char)

let sample (random: Random) (array: array<'a>) =
    let index = random.Next(Array.length array)
    Array.get array index

let readLines filename =
  IO.File.ReadAllLines(filename)

let intOfString (s: string): int =
  Int32.Parse(s)

let writeToFile filename s =
  IO.File.WriteAllText(filename, s)

let filesInDirectory dirName =
  IO.Directory.GetFiles(dirName)

let isNotEmpty s =
  String.length s > 0

let (./.) x y =
    ((x |> double) / (y |> double))

let chunkInto (n: int) seq =
  let size = Seq.length seq
  let chunkSize = ceil (size ./. n) |> int
  Seq.chunkBySize chunkSize seq |> Seq.map (Seq.ofArray)

let replaceInList (x: 'a) (y: 'a) (xs: 'a list): 'a list =
  let mapper a =
    if (a = x) then y else a

  List.map mapper xs

let replaceInString (a: string) (b: string) (s: string): string =
  s.Replace(a, b)

let nth int (a: 'a array): 'a =
  a.[int]

let nestedListsToSeqs ls =
  Seq.ofList (List.map Seq.ofList ls)

let assert2 condition description =
    if not condition
    then
        printfn "Failed assertion: %s" description
        exit 1
    else ()

let fileName (path: string): string =
  IO.Path.GetFileName path

let directoryName (path: string): string =
  IO.Path.GetDirectoryName path
