namespace SchedulingLab

module Serialization = 
    open DomainTypes
    open Utils

    module TaskProperties =
      let deserialize (s: string): TaskProperties =
        let numbers = splitOn " " s |> Array.map intOfString
        TaskProperties.create numbers.[0] numbers.[1] numbers.[2]

      let serialize (tp: TaskProperties) =
          [ tp.p; tp.r; tp.d ]
            |> List.map string
            |> joinWith " "


    module Task =
      let serialize (task: Task): string =
          TaskProperties.serialize task.properties


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
        let private machineToString (tasks: seq<Task>): string =
          tasks
          |> Seq.map Task.id
          |> Seq.map string
          |> joinWith " "

        let serialize (s: Solution): string =
            let latenessString = Solution.totalLateness s |> string
            let machinesStrings = Seq.map machineToString s |> joinWith "\n"

            [ latenessString; machinesStrings ] |> joinWith "\n"