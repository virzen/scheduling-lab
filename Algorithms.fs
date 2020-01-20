namespace SchedulingLab

open System

module Algorithms = 
    open DomainTypes
    open Utils 
    
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