namespace SchedulingLab

module DomainTypes =
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