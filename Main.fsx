let joinWith separator (iterable: seq<string>) =
  System.String.Join(separator, iterable)

module Task =
  type Task = {
    rj: int;
    pj: int;
    dj: int;
  }

  let toString (task: Task): string =
    [task.rj; task.pj; task.dj]
      |> List.map string
      |> joinWith " "

  let generateRandom (generator: System.Random) =
    {
      rj = (generator.Next(1, 10));
      pj = (generator.Next(1, 10));
      dj = (generator.Next(1, 10));
    }

module Instance =
  type Instance = {
    size: int;
    tasks: seq<Task.Task>;
  }

  let generate randomGenerator size =
    let tasks =
      seq { 1 .. size }
      |> Seq.map (fun _ -> Task.generateRandom randomGenerator)

    {
      size = size;
      tasks = tasks;
    }

  let toString (i: Instance) =
    let sizeString = string i.size
    let taskStrings = i.tasks |> Seq.map Task.toString |> List.ofSeq

    sizeString :: taskStrings |> joinWith "\n"


// MAIN
let n = 50

let randomGenerator = System.Random()

Instance.generate randomGenerator n
  |> Instance.toString
  |> printf "%A"


