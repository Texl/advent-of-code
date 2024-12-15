/// https://adventofcode.com/2024/day/15
module AdventOfCode2024.Day15

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day15.txt"

let testData =
   seq {
      "##########"
      "#..O..O.O#"
      "#......O.#"
      "#.OO..O.O#"
      "#..O@..O.#"
      "#O#..O...#"
      "#O..O..O.#"
      "#.OO.O.OO#"
      "#....O...#"
      "##########"
      ""
      "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
      "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
      "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
      "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
      "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
      "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
      ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
      "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
      "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
      "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
   }
   
let testData2 =
   seq {
      "########"
      "#..O.O.#"
      "##@.O..#"
      "#...O..#"
      "#.#.O..#"
      "#...O..#"
      "#......#"
      "########"
      ""
      "<^^>>>vv<v>>v<<"      
   }
   
let testData3 =
   seq {
      "#######"
      "#...#.#"
      "#.....#"
      "#..OO@#"
      "#..O..#"
      "#.....#"
      "#######"
      ""
      "<vv<<^^<<^^"      
   }
   
type GridCell =
   | Wall
   | Empty
   | Box
   | BoxLeft
   | BoxRight
   | Robot
   
type Direction =
   | Right
   | Left
   | Up
   | Down
   
let getGridAndDirections generateCells =
   data
   |> Seq.toList
   |> List.split String.isEmpty
   |> function
      | [ mapData; directionsData ] ->
         mapData
         |> Seq.map (Seq.collect generateCells)
         |> Seq.collecti (fun r -> Seq.mapi (fun c cell -> vec2i r c, cell))
         |> Map.ofSeq,

         directionsData
         |> String.concat ""
         |> Seq.map (function
            | '>' -> Right
            | '<' -> Left
            | '^' -> Up
            | 'v' -> Down
            | _ -> raise invalidInput)
         |> Seq.toList
      | _ -> raise invalidInput

let generateCells =
   function
   | '#' -> [ Wall ]
   | '.' -> [ Empty ]
   | 'O' -> [ Box ]
   | '@' -> [ Robot ]
   | _ -> raise invalidInput
   
let generateWidenedCells =
   function
   | '#' -> [ Wall; Wall ]
   | '.' -> [ Empty; Empty ]
   | 'O' -> [ BoxLeft; BoxRight ]
   | '@' -> [ Robot; Empty ]
   | _ -> raise invalidInput
   
let applyDirection (direction : Direction) (v : Vector2i) =
   match direction with
   | Right -> vec2i v.R (v.C + 1)
   | Left -> vec2i v.R (v.C - 1)
   | Up -> vec2i (v.R - 1) v.C
   | Down -> vec2i (v.R + 1) v.C

let rec tryMove (moverPosition : Vector2i) (direction : Direction) (grid : Map<Vector2i, GridCell>) =
   let mover = grid[moverPosition]
   let goalPosition = moverPosition |> applyDirection direction

   let applyMove grid =
      grid
      |> Map.add moverPosition Empty
      |> Map.add goalPosition mover
   
   opt {
      match grid[goalPosition] with
      | Wall ->
         return! None
      | Empty ->
         return applyMove grid
      | Box ->
         let! newGrid = tryMove goalPosition direction grid
         return applyMove newGrid
      | BoxLeft | BoxRight as boxHalf ->
         let! interimGrid = tryMove goalPosition direction grid
         if direction = Up || direction = Down then
            let otherBoxHalfPosition =
               let directionToOtherBoxHalf = if boxHalf = BoxLeft then Right else Left
               goalPosition |> applyDirection directionToOtherBoxHalf
            let! newGrid = interimGrid |> tryMove otherBoxHalfPosition direction
            return applyMove newGrid
         else
            return applyMove interimGrid
      | _ -> failwith "logic error"
   }

let solve generateCells =
   let mutable grid, directions = getGridAndDirections generateCells
   
   let mutable robotPosition =
      grid
      |> Map.toSeq
      |> Seq.find (fun (_, cell) -> cell = Robot)
      |> fst

   for dir in directions do
      match tryMove robotPosition dir grid with
      | Some newGrid ->
         grid <- newGrid
         robotPosition <- robotPosition |> applyDirection dir
      | None -> ()

   grid
   |> Map.toSeq
   |> Seq.sumBy (fun (p, c) ->
      match c with
      | Box | BoxLeft -> 100 * p.R + p.C
      | _ -> 0)

let part1 () = solve generateCells

let part2 () = solve generateWidenedCells

let part1Expected = 1485257

let part2Expected = 1475512
