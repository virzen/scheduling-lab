#load "Utils.fsx"
#load "Domain.fsx"

open System
open Utils
open Domain


let solveRandom (instance: Instance): Solution =
    let random = Random()

    Seq.groupBy (fun _ -> random.Next(1, 5)) instance
    |> Seq.map (fun (_, tasks) -> tasks)

let solveStatic (instance: Instance): Solution =
    let sizePerMachine = ((Seq.length instance) ./. 4) |> ceil |> int

    Seq.chunkBySize sizePerMachine instance
    |> Seq.map Seq.ofArray

let solveReference (instance: Instance): Solution =
  chunkInto 4 instance


module ListAlgorithm =
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

  let solveFirstFreeMachineByReadyTime (instance: Instance): Solution =
    let sortedTasks = Seq.sortBy Task.readyTime instance |> List.ofSeq
    assignToFirstFreeMachine sortedTasks


module AdvancedAlgoritm =
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

  let solveMonteCarlo (instance: Instance): Solution =
    let k = 2
    let aggr = mean

    let tasks = List.ofSeq instance
    let random = Random()

    let calculateCriteria (path: Task list): int =
      ListAlgorithm.assignToFirstFreeMachine path |> Solution.totalLateness

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

    ListAlgorithm.assignToFirstFreeMachine tasks



let indexNumber = 133865

let generateInstanceAndSolution n =
  let instance = Instance.generate n
  let solution: Solution = solveReference instance

  let inFilename = "in" + (string indexNumber) + "_" + (string n) + ".txt"
  let outFilename = "out" + (string indexNumber) + "_" + (string n) + ".txt"

  instance |> Instance.serialize |> writeToFile inFilename
  solution |> Solution.serialize |> writeToFile outFilename

let instanceFilenameToSortable (path: string) =
  let name = fileName path
  let withoutExtension = splitOn "." name |> nth 0
  let elements = splitOn "_" withoutExtension
  let paddedSize = padLeft '0' 3 elements.[1]

  sprintf "%s%s" elements.[0] paddedSize


let studentIdFromPath path =
  let name = fileName path
  let withoutExtension = splitOn "." name |> nth 0
  let basis = splitOn "_" withoutExtension

  basis.[0]

let filenameInToOut (s: string): string =
  replaceInString "in" "out" s

let isInputFile (path: string): bool =
  (fileName path).StartsWith "in"


let solveAllInDirectory solver dirName =
  let filenames =
    filesInDirectory dirName
    |> Array.filter isInputFile
    |> Array.sortBy instanceFilenameToSortable
  let instances = Array.map Instance.fromFile filenames
  let solutions = Array.map solver instances
  let latenesses = Array.map Solution.totalLateness solutions

  let solutionsPerStudent (id, pairs) =
    let latenesses = Array.map (fun (_filename, lateness) -> lateness) pairs |> Array.map string |> List.ofArray

    id::latenesses |> joinWith "\n"

  Array.zip filenames latenesses
  |> Array.groupBy (fun (a, _b) -> studentIdFromPath a)
  |> Array.map solutionsPerStudent
  |> joinWith "\n\n"
  |> printf "%s"


let measureSolving (path: string) =
  let name = fileName path
  let directory = directoryName path
  let outFileName = filenameInToOut name

  let instance = Instance.fromFile path

  let stopWatch = Diagnostics.Stopwatch.StartNew()
  let solution = ListAlgorithm.solveFirstFreeMachineByReadyTime instance
  stopWatch.Stop()

  solution
  |> Solution.serialize
  |> writeToFile (directory + "/rozwiazania/" + outFileName)

  stopWatch.Elapsed.TotalMilliseconds


let measureSolvingNTimes times path =
  let measurements = seq { 0 .. times } |> Seq.map (fun _ -> measureSolving path)
  let sum = Seq.sum measurements
  let mean = sum / (float (Seq.length measurements))

  mean

let f path =
  let meanMeasurement = measureSolvingNTimes 30 path

  path, meanMeasurement

let printResults (studentId, pairs) =
  let resultsString =
    pairs
    |> Seq.map (fun (_path, result) -> result)
    |> Seq.map string
    |> joinWith "\n"

  printfn "%s\n%s\n\n" studentId resultsString


let generateMeasurementsForAllInDirectory dir =
  filesInDirectory dir
  |> Array.filter isInputFile
  |> Array.sortBy instanceFilenameToSortable
  |> Array.map f
  |> Array.groupBy (fun (path, _result) -> studentIdFromPath path)
  |> Array.iter printResults


// MAIN

let instance = Instance.fromFile "moje-wrzucone/in133865_500.txt"

let result = instance
              |> AdvancedAlgoritm.solveMonteCarlo
              |> Solution.totalLateness

printfn "%A" result

