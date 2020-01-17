module Domain

#load "Utils.fsx"

open System
open Utils



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




module Task =
    let toString (task: Task): string =
        [ task.properties.p; task.properties.r; task.properties.d ]
          |> List.map string
          |> joinWith " "

    let id task =
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

    let deserialize (s: string): TaskProperties =
      let numbers = splitOn " " s |> Array.map intOfString

      create numbers.[0] numbers.[1] numbers.[2]

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

        let create (random: Random) rRange method count =
            let r = random.Next(1, rRange)

            let group =
                match method with
                | EDD -> generateForEDD random r
                | Longest -> generateForLongest random r

            Seq.take count group


    let fromFile (filename: string): Instance =
      let lines = readLines filename |> Array.filter isNotEmpty
      let totalTasks = lines.[0] |> intOfString
      let tasksProps = Array.map TaskProperties.deserialize lines.[1..]

      assert2 (totalTasks = Array.length tasksProps) |> ignore

      let tasks = Array.mapi (fun index props -> { id = index + 1; properties = props }) tasksProps

      Seq.ofArray tasks

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

    let serialize (i: Instance) =
        let sizeString = string (Seq.length i)

        let taskStrings =
            i
            |> Seq.map Task.toString
            |> List.ofSeq

        sizeString :: taskStrings
        |> joinWith "\n"

