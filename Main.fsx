open System

let joinWith separator (iterable: seq<string>) = String.Join(separator, iterable)

let sample (random: Random) (array: array<'a>) =
    let index = random.Next(Array.length array)
    Array.get array index

let writeToFile filename s =
  IO.File.WriteAllText(filename, s)

type TaskProperties =
    { p: int
      r: int
      d: int }

type Task =
    { id: int
      properties: TaskProperties }

type Instance = seq<Task>

type Solution =
  seq<seq<Task>>

let (./.) x y =
    (x |> double) / (y |> double)

let assert2 condition description =
    if not condition
    then
        printfn "Failed assertion: %s" description
        exit 1
    else ()

module Task =
    let toString (task: Task): string =
        [ task.properties.p; task.properties.r; task.properties.d ]
          |> List.map string
          |> joinWith " "

    let id task =
        task.id


module TaskProperties =

    let generateRandom (random: Random) =
        { p = (random.Next(1, 10))
          r = (random.Next(1, 10))
          d = (random.Next(1, 10)) }

    let create (p: int) (r: int) (d: int): TaskProperties =
        assert2 (p >= 1) "task must have non-zero length" |> ignore
        assert2 (p <= (d - r)) "task must doable within ready time and due date" |> ignore
        assert2 (d > r) "ready time must be before due date" |> ignore

        { p = p; r = r; d = d }

    let toPrettyString task = String.replicate task.r " " + String.replicate task.p "X"

    let printMany (tasks: seq<TaskProperties>) =
        tasks
        |> Seq.map toPrettyString
        |> joinWith "\n"
        |> printf "\n%s\n"





module Instance =
    let generateFullyRandomly random size =
        seq { 1 .. size }
        |> Seq.map (fun n -> { id = n; properties = TaskProperties.generateRandom random })

    module Group =
        type GenerationMethod =
            | EDD
            | Longest

        let methods = [| EDD; Longest |]

        let generateNumbersSummingTo (random: Random) sum =
            assert2 (sum >= 2) "sum must be at least 2"

            let rand = random.Next(1, sum - 1)
            let a = rand
            let b = sum - rand

            (a, b)

        let generateForEDD (random: Random) r =
            let generateTriple _ =
                let pi = random.Next(2, 10)
                let (pi1, pi2) = (generateNumbersSummingTo random pi)
                let d = r + pi

                [ (TaskProperties.create pi r d)
                  (TaskProperties.create pi1 r d)
                  (TaskProperties.create pi2 r d ) ]

            seq { 1 .. 2 }
            |> Seq.map generateTriple
            |> List.concat
            |> List.toSeq



        let generateForLongest (random: Random) r =
            let generatePair _ =
                let pi = random.Next(3, 10)
                let pi1 = random.Next(1, pi - 1)
                let a = (r + pi + pi1)
                let di = random.Next(a, a + 10)
                let di1 = random.Next(r + pi1, a)

                [ (TaskProperties.create pi r di)
                  (TaskProperties.create pi1 r di1) ]

            seq { 1 .. 3 }
            |> Seq.map generatePair
            |> List.concat
            |> List.toSeq

        let create (random: Random) method count =
            let r = random.Next(1, 10)

            let group =
                match method with
                | EDD -> generateForEDD random r
                | Longest -> generateForLongest random r

            Seq.take count group




    // TODO: condense
    let generate (random: Random) instanceSize: Instance =
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

        groupCounts
        |> Seq.chunkBySize groupCountsChunkSize
        |> Seq.zip Group.methods
        |> Seq.map generateGroupsWithMethod
        |> Seq.concat
        |> Seq.concat
        |> Seq.mapi (fun index taskProps -> { id = index; properties = taskProps })

    let serialize (i: Instance) =
        let sizeString = string (Seq.length i)

        let taskStrings =
            i
            |> Seq.map Task.toString
            |> List.ofSeq

        sizeString :: taskStrings
        |> joinWith "\n"

module Solution =
    let solveRandom (random: Random) (instance: Instance): Solution =
        Seq.groupBy (fun _ -> random.Next(1, 5)) instance
        |> Seq.map (fun (_, tasks) -> tasks)

    let solveStatic (instance: Instance): Solution =
        let sizePerMachine = ((Seq.length instance) ./. 4) |> ceil |> int

        Seq.chunkBySize sizePerMachine instance
        |> Seq.map Seq.ofArray

    type Accumulator =
      { lastEnd: int; lateness: int }

    let latenessPerMachine (tasks: seq<Task>) =
        let accumulate acc task =
          let startTime = max acc.lastEnd task.properties.r
          let endTime = startTime + task.properties.p
          let lateness = max 0 endTime - task.properties.d

          { lastEnd = endTime; lateness = acc.lateness + lateness }

        let result = Seq.fold accumulate { lastEnd = 0; lateness = 0 } tasks

        result.lateness

    let totalLateness (s: Solution) =
        Seq.map latenessPerMachine s |> Seq.sum

    let serialize (s: Solution): string =
        let machineToString (tasks: seq<Task>): string =
          Seq.map (fun task -> task.id) tasks |> Seq.map string |> joinWith " "

        let latenessString = totalLateness s |> string
        let machinesStrings = Seq.map machineToString s |> joinWith "\n"

        [ latenessString; machinesStrings ] |> joinWith "\n"


// MAIN
let indexNumber = 133865
let randomGenerator = Random()

let generateInstanceAndSolution n =
  let instance = Instance.generate randomGenerator n
  let solution: Solution = Solution.solveRandom randomGenerator instance

  let inFilename = "in" + (string indexNumber) + "_" + (string n) + ".txt"
  let outFilename = "out" + (string indexNumber) + "_" + (string n) + ".txt"

  instance |> Instance.serialize |> writeToFile inFilename
  solution |> Solution.serialize |> writeToFile outFilename


generateInstanceAndSolution 50
