namespace SchedulingLab

module Program = 
  [<EntryPoint>]
  let main argv =
      let instance = Serialization.Instance.fromFile "../../../moje-wrzucone/in133865_500.txt"

      let result = instance
                    |> Algorithms.Advanced.run
                    |> Solution.totalLateness

      printfn "%A" result

      0 // return an integer exit code
