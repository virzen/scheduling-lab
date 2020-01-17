#load "Utils.fsx"
#load "Domain.fsx"

open System
open Utils
open Domain


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





module Solution =
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

    let ofMachineList (machines: Machine list): Solution =
       machines
       |> List.sortBy Machine.id
       |> List.map Machine.tasks
       |> nestedListsToSeqs


    let solveFirstFreeMachineByReadyTime (instance: Instance): Solution =
      let sortedTasks = Seq.sortBy Task.readyTime instance |> List.ofSeq

      let firstReadyMachine machines = List.minBy Machine.lastEnd machines

      let rec step machines tasksLeft =
        match tasksLeft with
        | [] -> machines
        | task::rest ->
            let selectedMachine = firstReadyMachine machines
            let withTask = Machine.addTask task selectedMachine
            let newMachines = replaceInList selectedMachine withTask machines

            step newMachines rest

      let initialMachines = [(Machine.empty 1); (Machine.empty 2); (Machine.empty 3); (Machine.empty 4)]

      step initialMachines sortedTasks |> ofMachineList



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

    let serialize (s: Solution): string =
        let machineToString (tasks: seq<Task>): string =
          Seq.map Task.id tasks |> Seq.map string |> joinWith " "

        let latenessString = totalLateness s |> string
        let machinesStrings = Seq.map machineToString s |> joinWith "\n"

        [ latenessString; machinesStrings ] |> joinWith "\n"


// MAIN
let indexNumber = 133865

let generateInstanceAndSolution n =
  let instance = Instance.generate n
  let solution: Solution = Solution.solveReference instance

  let inFilename = "in" + (string indexNumber) + "_" + (string n) + ".txt"
  let outFilename = "out" + (string indexNumber) + "_" + (string n) + ".txt"

  instance |> Instance.serialize |> writeToFile inFilename
  solution |> Solution.serialize |> writeToFile outFilename

let fileName (path: string): string =
  IO.Path.GetFileName path

let directoryName (path: string): string =
  IO.Path.GetDirectoryName path


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
  let solution = Solution.solveFirstFreeMachineByReadyTime instance
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

solveAllInDirectory Solution.solveReference "wszystkie-wrzucone"
