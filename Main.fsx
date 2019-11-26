let joinWith separator (iterable: seq<string>) = System.String.Join(separator, iterable)

let sample (random: System.Random) (array: array<'a>) =
    let index = random.Next(Array.length array)
    Array.get array index

type Task =
    { p: int
      r: int
      d: int }

type Instance = seq<Task>

let assert2 condition description =
    if not condition
    then
        printfn "Failed assertion: %s" description
        exit 1
    else ()

module Task =

    let toString (task: Task): string =
        [ task.p; task.r; task.d ]
        |> List.map string
        |> joinWith " "

    let generateRandom (random: System.Random) =
        { p = (random.Next(1, 10))
          r = (random.Next(1, 10))
          d = (random.Next(1, 10)) }

    let create (p: int) (r: int) (d: int): Task =
        assert2 (p >= 1) "task must have non-zero length" |> ignore
        assert2 (p <= (d - r)) "task must doable within ready time and due date" |> ignore
        assert2 (d > r) "ready time must be before due date" |> ignore

        { p = p; r = r; d = d }

    let empty =
        { p = 0
          r = 0
          d = 0 }

    let toPrettyString task = String.replicate task.r " " + String.replicate task.p "X"

    let printMany (tasks: seq<Task>) =
        tasks
        |> Seq.map toPrettyString
        |> joinWith "\n"
        |> printf "\n%s\n"





module Instance =
    let generateFullyRandomly random size =
        let tasks = seq { 1 .. size } |> Seq.map (fun _ -> Task.generateRandom random)

        tasks

    module Group =
        type GenerationMethod =
            | EDD
            | Longest

        let methods = [| EDD; Longest |]

        let generateNumbersSummingTo (random: System.Random) sum =
            assert2 (sum >= 2) "sum must be at least 2"

            let rand = random.Next(1, sum - 1)
            let a = rand
            let b = sum - rand

            (a, b)

        let generateForEDD (random: System.Random) r =
            let generateTriple _ =
                let pi = random.Next(2, 10)
                let (pi1, pi2) = (generateNumbersSummingTo random pi)
                let d = r + pi

                [ (Task.create pi r d)
                  (Task.create pi1 r d)
                  (Task.create pi2 r d ) ]

            seq { 1 .. 2 }
            |> Seq.map generateTriple
            |> List.concat
            |> List.toSeq



        let generateForLongest (random: System.Random) r: Instance =
            let generatePair _ =
                let pi = random.Next(3, 10)
                let pi1 = random.Next(1, pi - 1)
                let a = (r + pi + pi1)
                let di = random.Next(a, a + 10)
                let di1 = random.Next(r + pi1, a)

                [ (Task.create pi r di)
                  (Task.create pi1 r di1) ]

            seq { 1 .. 3 }
            |> Seq.map generatePair
            |> List.concat
            |> List.toSeq




        let create (random: System.Random) method count: Instance =
            let r = random.Next(1, 10)

            let group =
                match method with
                | EDD -> generateForEDD random r
                | Longest -> generateForLongest random r

            Seq.take count group




    // TODO: condense
    let generate (random: System.Random) instanceSize =
        let generateGroupsWithMethod (method, groupCounts) =  Seq.map (Group.create random method) groupCounts

        let groupSize = 6

        let groupCounts =
            seq { 1 .. instanceSize }
            |> Seq.chunkBySize groupSize
            |> Seq.map (Array.length)

        let groupCountsChunkSize =
            [ instanceSize
              (Array.length Group.methods)
              6 ]
            |> List.map float
            |> List.reduce (fun a b -> ceil (a / b))
            |> int

        let tasks =
            groupCounts
            |> Seq.chunkBySize groupCountsChunkSize
            |> Seq.zip Group.methods
            |> Seq.map generateGroupsWithMethod
            |> Seq.concat
            |> Seq.concat

        tasks

    let toString (i: Instance) =
        let sizeString = string (Seq.length i)

        let taskStrings =
            i
            |> Seq.map Task.toString
            |> List.ofSeq

        sizeString :: taskStrings
        |> joinWith "\n"


// MAIN
let n = 50

let randomGenerator = System.Random()

Instance.generate randomGenerator n
|> Instance.toString
|> printf "%s"
