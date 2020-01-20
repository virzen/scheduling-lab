open System

// DOMAIN TYPES

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

// UTILS

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

// DOMAIN OPERATIONS

module Task = 
    let id (task: Task) =
        task.id

    let readyTime task =
        task.properties.r


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

        let create (random: Random) rRange method count =
            let r = random.Next(1, rRange)

            let group =
                match method with
                | EDD -> generateForEDD random r
                | Longest -> generateForLongest random r

            Seq.take count group


    // TODO: condense
    let generate instanceSize: Instance =
        let random = Random()
        let generateGroupsWithMethod (method, groupCounts) =  Seq.map (Group.create random instanceSize method) groupCounts

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
        |> Seq.mapi (fun index taskProps -> { id = index + 1; properties = taskProps })

module Solution =
    type Accumulator =
      { lastEnd: int; lateness: int }

    let latenessPerMachine (tasks: seq<Task>) =
        let accumulate acc task =
          let startTime = max acc.lastEnd task.properties.r
          let endTime = startTime + task.properties.p
          let lateness = max 0 (endTime - task.properties.d)

          { lastEnd = endTime; lateness = acc.lateness + lateness }

        let result = Seq.fold accumulate { lastEnd = 0; lateness = 0 } tasks

        result.lateness

    let totalLateness (s: Solution) =
        Seq.map latenessPerMachine s |> Seq.sum


// ALGORITHMS

module Algorithms = 
    module Random =
      let run (instance: Instance): Solution =
          let random = Random()

          Seq.groupBy (fun _ -> random.Next(1, 5)) instance
          |> Seq.map (fun (_, tasks) -> tasks)

    module Static =
      let run (instance: Instance): Solution =
          let sizePerMachine = ((Seq.length instance) ./. 4) |> ceil |> int

          Seq.chunkBySize sizePerMachine instance
          |> Seq.map Seq.ofArray

    module Reference =
      let run (instance: Instance): Solution =
        chunkInto 4 instance

    module List =
      type Machine =
        { id: int
          tasks: Task list }

      module Machine =
        let addTask t m =
          let newTasks = List.append m.tasks [t]

          { m with tasks = newTasks }

        let empty (id: int): Machine =
          { id = id; tasks = [] }

        let lastEnd (machine: Machine): int =
          List.fold (fun last task -> (max last task.properties.r) + task.properties.p) 0 machine.tasks

        let id (m: Machine) =
          m.id

        let tasks (m: Machine) =
          m.tasks

      let machinesToSolution (machines: Machine list): Solution =
         machines
         |> List.sortBy Machine.id
         |> List.map Machine.tasks
         |> nestedListsToSeqs

      let assignToFirstFreeMachine (tasks: Task list): Solution =
        let firstReadyMachine machines = List.minBy Machine.lastEnd machines

        let rec step machines tasksLeft =
          match tasksLeft with
          | [] -> machines
          | task::rest ->
              let selectedMachine = firstReadyMachine machines
              let machineWithTask = Machine.addTask task selectedMachine
              let newMachines = replaceInList selectedMachine machineWithTask machines

              step newMachines rest

        let initialMachines = [(Machine.empty 1); (Machine.empty 2); (Machine.empty 3); (Machine.empty 4)]

        step initialMachines tasks |> machinesToSolution

      let run (instance: Instance): Solution =
        let sortedTasks = Seq.sortBy Task.readyTime instance |> List.ofSeq
        assignToFirstFreeMachine sortedTasks


    module Advanced =
      let swap (a: _[]) x y =
          let tmp = a.[x]
          a.[x] <- a.[y]
          a.[y] <- tmp

      // shuffle an array (in-place)
      let shuffle (rand: Random) a =
          Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a

      let shuffleList (rand: Random) l =
          let mutable xs = Array.ofList l
          shuffle rand xs
          List.ofArray xs

      let permutations (random: Random) (k: int) (l: 'a list): seq<'a list> =
        seq { 1..k } |> Seq.map (fun _ -> shuffleList random l)

      let removeTask (x: Task) (l: Task list): Task list =
        List.filter (fun t -> t <> x) l

      let mean (xs: seq<int>): float =
        (Seq.sum xs) ./. (Seq.length xs)

      let run (instance: Instance): Solution =
        let k = 2
        let aggr = mean

        let tasks = List.ofSeq instance
        let random = Random()

        let calculateCriteria (path: Task list): int =
          List.assignToFirstFreeMachine path |> Solution.totalLateness

        let calculateNodeValue fixedNodes nodesLeft (task: Task): float =
          let permutations = permutations random k (removeTask task nodesLeft)
          let fullPaths = Seq.map (List.append fixedNodes) permutations
          let criteria = Seq.map calculateCriteria fullPaths
          aggr criteria

        let rec step (fixedNodes: Task list) (nodesLeft: Task list): Task list =
          if (List.length nodesLeft) = 0
          then
            fixedNodes
          else
            printfn "%A" fixedNodes
            let maxValueNode = List.minBy (calculateNodeValue fixedNodes nodesLeft) nodesLeft

            let nextFixedNodes = List.append fixedNodes [maxValueNode]
            let nextNodesLeft = removeTask maxValueNode nodesLeft

            step nextFixedNodes nextNodesLeft

        let tasks = step [] tasks

        List.assignToFirstFreeMachine tasks

// INFRASTRUCTURE

module Serialization = 
  module TaskProperties =
      let deserialize (s: string): TaskProperties =
        let numbers = splitOn " " s |> Array.map intOfString
        TaskProperties.create numbers.[0] numbers.[1] numbers.[2]

  module Task =
      let serialize (task: Task): string =
          [ task.properties.p; task.properties.r; task.properties.d ]
            |> List.map string
            |> joinWith " "

  module Instance =
      let fromFile (filename: string): Instance =
        let lines = readLines filename |> Array.filter isNotEmpty
        let totalTasks = lines.[0] |> intOfString
        let tasksProps = Array.map TaskProperties.deserialize lines.[1..]

        assert2 (totalTasks = Array.length tasksProps) |> ignore

        let tasks = Array.mapi (fun index props -> { id = index + 1; properties = props }) tasksProps

        Seq.ofArray tasks

      
      let serialize (i: Instance) =
          let sizeString = string (Seq.length i)

          let taskStrings =
              i
              |> Seq.map Task.serialize
              |> List.ofSeq

          sizeString :: taskStrings
          |> joinWith "\n"

    module Solution =
        let serialize (s: Solution): string =
            let machineToString (tasks: seq<Task>): string =
              Seq.map Task.id tasks |> Seq.map string |> joinWith " "

            let latenessString = Solution.totalLateness s |> string
            let machinesStrings = Seq.map machineToString s |> joinWith "\n"

            [ latenessString; machinesStrings ] |> joinWith "\n"


module Logging =
  module TaskProperties =
      let toPrettyString task = String.replicate task.r " " + String.replicate task.p "X"

      let printMany (tasks: seq<TaskProperties>) =
          tasks
          |> Seq.map toPrettyString
          |> joinWith "\n"
          |> printf "\n%s\n"


// MAIN
[<EntryPoint>]
let main argv =
    let instance = Serialization.Instance.fromFile "../../../moje-wrzucone/in133865_500.txt"

    let result = instance
                  |> Algorithms.Advanced.run
                  |> Solution.totalLateness

    printfn "%A" result

    0 // return an integer exit code
