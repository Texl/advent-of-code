/// https://adventofcode.com/2023/day/10
module AdventOfCode2023.Day10

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day10.txt"

type Direction =
   | N
   | S
   | E
   | W
   with
   static member AllDirections = [| N; S; E; W |]
   member this.Vector =
      match this with
      | N -> -Vector2.UnitR
      | S -> Vector2.UnitR
      | E -> Vector2.UnitC
      | W -> -Vector2.UnitC
   member this.Opposite =
      match this with
      | N -> S
      | S -> N
      | E -> W
      | W -> E

type Tile =
   | EmptyTile of Vector2
   | StartTile of Vector2
   | Tile of Vector2 * Direction list
   with
   member this.Coords =
      match this with
      | EmptyTile coords -> coords
      | StartTile coords -> coords
      | Tile (coords, _) -> coords
   
   member this.Directions =
      match this with
      | EmptyTile _ -> Some []
      | StartTile _ -> None
      | Tile (_, directions) -> Some directions
   member this.ContainsDirection (direction : Direction) =
      this.Directions
      |> Option.exists (List.contains direction)


type Grid (data : seq<string>) =
   let tileGrid =
      data
      |> Seq.mapi (fun r row ->
         row
         |> Seq.mapi (fun c ->
            let coords = { R = r; C = c }
            function
            | '.' -> EmptyTile coords
            | 'S' -> StartTile coords
            | '|' -> Tile (coords, [ N; S ])
            | '-' -> Tile (coords, [ E; W ])
            | 'L' -> Tile (coords, [ N; E ])
            | 'J' -> Tile (coords, [ N; W ])
            | '7' -> Tile (coords, [ S; W ])
            | 'F' -> Tile (coords, [ S; E ])
            | _ -> raise invalidInput)
         |> Array.ofSeq)
      |> Array.ofSeq

   let startTile =
      tileGrid
      |> Seq.collect (Seq.filter (function StartTile _ -> true | _ -> false))
      |> Seq.exactlyOne

   let tryGetTile coords =
      tileGrid
      |> Array.tryItem (int coords.R)
      |> Option.bind (Array.tryItem (int coords.C))
   
   let tryStepDirection (direction : Direction) (tile : Tile) =
      tryGetTile (tile.Coords + direction.Vector)

   let getTileDirections =
      let startTileDirections =
         Direction.AllDirections
         |> Seq.filter (fun direction ->
            startTile
            |> tryStepDirection direction
            |> Option.exists (fun neighbor -> neighbor.ContainsDirection direction.Opposite))
         |> List.ofSeq

      function
      | EmptyTile _ -> []
      | StartTile _ -> startTileDirections
      | Tile (_, dirs) -> dirs

   let step (currentTile : Tile) =
      currentTile
      |> getTileDirections
      |> List.choose (fun direction -> currentTile |> tryStepDirection direction)

   let rec getPath (headTiles : Tile list) (pathTileToDist : Map<_,_>) (dist : int) grid =
      let newPathTileToDist =
         (pathTileToDist, headTiles)
         ||> Seq.fold (fun s n -> Map.add n dist s)
      
      match
         headTiles
         |> Seq.collect step
         |> Seq.filter (flip Map.containsKey newPathTileToDist >> not)
         |> List.ofSeq
         with
      | [] ->
         {| Tiles = newPathTileToDist |> Map.keys |> Set.ofSeq
            Length = newPathTileToDist |> Map.values |> Seq.max |} 
      | newHeads -> getPath newHeads newPathTileToDist (dist + 1) grid

   let getEnclosedTiles (pathTiles : Set<Tile>) =
      tileGrid
      |> Seq.collect (fun row ->
         row
         |> Seq.tails
         |> Seq.choose (fun tail ->
            match tail |> List.ofSeq with
            | headTile :: _ when pathTiles.Contains headTile -> None
            | headTile :: rest ->
               let parity =
                  rest
                  |> Seq.filter pathTiles.Contains
                  |> Seq.filter (fun tile -> tile |> getTileDirections |> Seq.contains N)
                  |> Seq.length
               if parity % 2 <> 0 then Some headTile else None
            | [] -> None))
      |> Set.ofSeq

   let toString (pathTiles : Set<Tile>) (enclosedTiles : Set<Tile>) =
      System.Console.OutputEncoding <- System.Text.Encoding.UTF8
      tileGrid
      |> Seq.map (fun row ->
         row
         |> Seq.map (fun tile ->
            if pathTiles.Contains tile then
               // tiles in path
               match tile with
               | StartTile _ -> "⌂"
               | Tile (_, [ E; W ]) -> "═"
               | Tile (_, [ N; S ]) -> "║"
               | Tile (_, [ N; E ]) -> "╚"
               | Tile (_, [ N; W ]) -> "╝"
               | Tile (_, [ S; W ]) -> "╗"
               | Tile (_, [ S; E ]) -> "╔"
               | _ -> failwith "unexpected"
            elif enclosedTiles.Contains tile then
               // tiles enclosed by path
               match tile with
               | EmptyTile _ -> "░"
               | Tile (_, [ E; W ]) -> "─"
               | Tile (_, [ N; S ]) -> "│"
               | Tile (_, [ N; E ]) -> "└"
               | Tile (_, [ N; W ]) -> "┘"
               | Tile (_, [ S; W ]) -> "┐"
               | Tile (_, [ S; E ]) -> "┌"
               | _ -> failwith "unexpected"
            else
               // tiles not in or enclosed by path
               match tile with
               | EmptyTile _ -> "."
               | Tile (_, [ E; W ]) -> "-"
               | Tile (_, [ N; S ]) -> "|"
               | Tile (_, [ N; E ]) -> "L"
               | Tile (_, [ N; W ]) -> "J"
               | Tile (_, [ S; W ]) -> "7"
               | Tile (_, [ S; E ]) -> "F"
               | _ -> failwith "unexpected")
         |> String.concat "")
      |> String.concat "\n"

   with
   member this.GetPath () =
      getPath [ startTile ] Map.empty 0 tileGrid

   member this.GetEnclosedTiles () =
      let pathTiles = this.GetPath() |> (_.Tiles)
      getEnclosedTiles pathTiles

   member this.Print () =
      let pathTiles = this.GetPath () |> (_.Tiles)
      let enclosedTiles = this.GetEnclosedTiles ()
      System.Console.OutputEncoding <- System.Text.Encoding.UTF8
      printfn $"%s{(toString pathTiles enclosedTiles)}"

let grid = Grid data

let part1 () = grid.GetPath() |> (_.Length)

let part2 () = grid.GetEnclosedTiles() |> Set.count

let part1Expected = 7_063

let part2Expected = 589
