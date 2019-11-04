let joinWith separator (iterable: seq<string>) = System.String.Join(separator, iterable)

let sample (random: System.Random) (array: array<'a>) =
    let index = random.Next(Array.length array)
    Array.get array index

module Task =
    type T =
        { r: int
          p: int
          d: int }

    let toString (task: T): string =
        [ task.r; task.p; task.d ]
        |> List.map string
        |> joinWith " "

    let generateRandom (random: System.Random) =
        { r = (random.Next(1, 10))
          p = (random.Next(1, 10))
          d = (random.Next(1, 10)) }

    let create r p d: T =
        { r = r
          p = p
          d = d }

    let empty =
        { r = 0
          p = 0
          d = 0 }

    let toPrettyString task = String.replicate task.r " " + String.replicate task.p "X"

    let printMany (tasks: seq<T>) =
        tasks
        |> Seq.map toPrettyString
        |> joinWith "\n"
        |> printf "\n%s\n"





module Instance =
    type T =
        // TODO: refactor not to use size
        { size: int
          tasks: seq<Task.T> }

    let generateFullyRandomly random size =
        let tasks = seq { 1 .. size } |> Seq.map (fun _ -> Task.generateRandom random)

        { size = size
          tasks = tasks }

    module Group =
        type GenerationMethod =
            | EDD
            | Longest

        let methods = [| EDD; Longest |]

        type T = seq<Task.T>

        let generateForEDD (random: System.Random) r =
            let generateTriple _ =
                let rand = random.Next(1, 10)
                let pi = rand
                let rand2 = random.Next(1, pi)
                let pi1 = pi - rand2
                let pi2 = pi - pi1
                let d = r + pi

                let task1 = Task.create r pi d
                let task2 = Task.create r pi1 d
                let task3 = Task.create r pi2 d

                [ task1; task2; task3 ]

            seq { 1 .. 2 }
            |> Seq.map generateTriple
            |> List.concat
            |> List.toSeq



        let generateForLongest (random: System.Random) r: T =
            let generatePair _ =
                let pi = random.Next(3, 10)
                let pi1 = random.Next(1, pi)
                let a = (r + pi + pi1)
                let di = random.Next(a, a + 10)
                let di1 = random.Next(a + 1)

                let task1 = Task.create r pi di

                let task2 = Task.create r pi1 di1

                [ task1; task2 ]

            seq { 1 .. 3 }
            |> Seq.map generatePair
            |> List.concat
            |> List.toSeq




        let create (random: System.Random) method count: T =
            let r = random.Next(1, 10)

            let group =
                match method with
                | EDD -> generateForEDD random r
                | Longest -> generateForLongest random r

            group |> Seq.take count





    let generate (random: System.Random) instanceSize =
        let generateGroupsWithMethod (method, groupCounts) = groupCounts |> Seq.map (Group.create random method)

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

        { size = instanceSize
          tasks = tasks }





    let toString (i: T) =
        let sizeString = string i.size

        let taskStrings =
            i.tasks
            |> Seq.map Task.toString
            |> List.ofSeq

        sizeString :: taskStrings
        |> joinWith "\n"


// MAIN
let n = 50

let randomGenerator = System.Random()

Instance.generate randomGenerator n
|> Instance.toString
|> printf "%A"
