/// https://adventofcode.com/2024/day/16
module AdventOfCode2024.Day16

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day16.txt"

let testData =
   seq {
      "###############"
      "#.......#....E#"
      "#.#.###.#.###.#"
      "#.....#.#...#.#"
      "#.###.#####.#.#"
      "#.#.#.......#.#"
      "#.#.#####.###.#"
      "#...........#.#"
      "###.#.#####.#.#"
      "#...#.....#.#.#"
      "#.#.#.###.#.#.#"
      "#.....#...#.#.#"
      "#.###.#.#.#.#.#"
      "#S..#.....#...#"
      "###############"
   }
   
type GridCell =
   | Start
   | End
   | Empty
   | Wall
   
type Facing =
   | North
   | South
   | East
   | West

type Pose =
   { Position : Vector2i
     Facing : Facing }
   member this.Step() =
      match this.Facing with
      | North -> { this with Position = this.Position - Vector2i.UnitR }
      | South -> { this with Position = this.Position + Vector2i.UnitR }
      | East -> { this with Position = this.Position + Vector2i.UnitC }
      | West -> { this with Position = this.Position - Vector2i.UnitC }

   member this.Left() =
      match this.Facing with
      | North -> { this with Facing = West }
      | South -> { this with Facing = East }
      | East -> { this with Facing = North }
      | West -> { this with Facing = South }

   member this.Right() =
      match this.Facing with
      | North -> { this with Facing = East }
      | South -> { this with Facing = West }
      | East -> { this with Facing = South }
      | West -> { this with Facing = North }

type State =
   { Pose : Pose
     Score : int
     PathR : Vector2i list }
   member this.Step() =
      let nextPose = this.Pose.Step()
      { Pose = nextPose
        Score = this.Score + 1
        PathR = nextPose.Position :: this.PathR }
   member this.Left() =
      { Pose = this.Pose.Left()
        Score = this.Score + 1000
        PathR = this.PathR }
   member this.Right() =
      { Pose = this.Pose.Right()
        Score = this.Score + 1000
        PathR = this.PathR }

let grid =
   data
   |> Seq.collecti (fun r ->
      Seq.mapi (fun c cell ->
         vec2i r c,
         match cell with
         | 'S' -> Start
         | 'E' -> End
         | '#' -> Wall
         | '.' -> Empty
         | _ -> raise invalidInput))
   |> Map.ofSeq

let search (initialHead : State) =
   let mutable bestScore = System.Int32.MaxValue
   let mutable bestPaths = []
   let mutable minScores = Map.empty

   let rec solve (queue : State list) =
      match queue with
      | state :: remaining ->
         if state.Score > bestScore then
            solve remaining
         elif grid[state.Pose.Position] = End then
            if state.Score = bestScore then
               bestPaths <- (state.PathR |> List.rev) :: bestPaths
            elif state.Score < bestScore then
               bestScore <- state.Score
               bestPaths <- [ state.PathR |> List.rev ]
               
            solve remaining
         else
            let newStates =
               let step = state.Step()
               [ if grid[step.Pose.Position] <> Wall then step
                 state.Left()
                 state.Right() ]
               |> List.choose (fun state ->
                  let minPoseScore =
                     minScores
                     |> Map.tryFind state.Pose
                     |> Option.defaultValue System.Int32.MaxValue
                  
                  if state.Score <= minPoseScore then
                     minScores <- minScores |> Map.add state.Pose state.Score
                     Some state
                  else
                     None)

            solve (remaining @ newStates)
       | [] -> bestScore, bestPaths

   solve [ initialHead ]

let initialState =
   let startPosition =
      grid
      |> Map.toSeq
      |> Seq.find (fun (_, cell) -> cell = Start)
      |> fst

   { Pose = { Position = startPosition; Facing = East }
     Score = 0
     PathR = [ startPosition ] }

let solution = lazy search initialState

let part1 () = solution.Value |> fst

let part2 () = solution.Value |> snd |> Seq.collect id |> Seq.distinct |> Seq.length

let part1Expected = 94436

let part2Expected = 481
