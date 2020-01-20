namespace SchedulingLab

module Logging =
    open DomainTypes

    module TaskProperties =
      let toPrettyString task = String.replicate task.r " " + String.replicate task.p "X"

      let printMany (tasks: seq<TaskProperties>) =
          tasks
          |> Seq.map toPrettyString
          |> Utils.joinWith "\n"
          |> printf "\n%s\n"