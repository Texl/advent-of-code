/// https://adventofcode.com/2023/day/23
module AdventOfCode2023.Day23

open AdventOfCode.Common

let testData =
   [ "#.#####################"
     "#.......#########...###"
     "#######.#########.#.###"
     "###.....#.>.>.###.#.###"
     "###v#####.#v#.###.#.###"
     "###.>...#.#.#.....#...#"
     "###v###.#.#.#########.#"
     "###...#.#.#.......#...#"
     "#####.#.#.#######.#.###"
     "#.....#.#.#.......#...#"
     "#.#####.#.#.#########v#"
     "#.#...#...#...###...>.#"
     "#.#.#v#######v###.###v#"
     "#...#.>.#...>.>.#.###.#"
     "#####v#.#.###v#.#.###.#"
     "#.....#...#...#.#.#...#"
     "#.#########.###.#.#.###"
     "#...###...#...#...#.###"
     "###.###.#.###v#####v###"
     "#...#...#.#.>.>.#.>.###"
     "#.###.###.#.###.#.#v###"
     "#.....###...###...#...#"
     "#####################.#" ]

let data = EmbeddedResource.loadText "Data/Day23.txt"

type Direction =
   | N
   | W
   | S
   | E

type Tile =
   | Empty
   | Path
   | Slope of Direction
   with
   static member Parse ch =
      match ch with
      | '#' -> Empty
      | '.' -> Path
      | '^' -> Slope N
      | '<' -> Slope W
      | 'v' -> Slope S
      | '>' -> Slope E
      | _ -> raise invalidInput

let grid2 = data |> Seq.map (Seq.map Tile.Parse >> Array.ofSeq) |> Array.ofSeq

let height = grid2.Length
let width = grid2[0].Length
let tileAt (v : Vector2i) = grid2[v.R][v.C]
let start2 : Vector2i = { R = 0; C = grid2 |> Array.head |> Array.findIndex ((=) Path) }
let finish2 : Vector2i = { R = height - 1; C = grid2 |> Array.last |> Array.findIndex ((=) Path) }

let junctions2 =
   [| for r in 1 .. height - 2 do
         for c in 1 .. width - 2 do
            let isJunction =
               [| grid2[r - 1][c]
                  grid2[r ][c - 1]
                  grid2[r + 1][c]
                  grid2[r ][c + 1] |]
               |> Array.forall (function
                  | Empty | Slope _ -> true
                  | _ -> false)
               
            if isJunction then
               yield ({ R = r; C = c } : Vector2i) |]

let isJunction (v : Vector2i) = junctions2 |> Array.contains v

let printSubPath sp =
   printfn "%A -> %A" (sp |> List.head) (sp |> List.last)

let pathSegments =
   let rec gather paths accR (stack : Vector2i list) =
      match stack with
      [] ->
         paths |> List.rev
      | v :: remaining ->
         let neighbors =
            [| if v.R > 0 then
                  let nv = { v with R = v.R - 1 }
                  N, nv, tileAt nv
               if v.C > 0 then
                  let wv = { v with C = v.C - 1 }
                  W, wv, tileAt wv
               if v.R < height - 1 then
                  let sv = { v with R = v.R + 1 }
                  S, sv, tileAt sv
               if v.C < width - 1 then
                  let ev = { v with C = v.C + 1 }
                  E, ev, tileAt ev |]
            |> Array.filter (fun (_, _, t) -> t <> Empty)

         let pathEnding =
            v = finish2 ||
            v |> isJunction

         if pathEnding then
            // printfn "ending at %A" v 
            
            let path = v :: accR |> List.rev

            let queueThese =
               neighbors
               |> Seq.filter (fun (nd, nv, nt) ->
                  let pathable =
                     match nt with
                     | Path -> true
                     | Slope _ -> true // sd when sd = nd -> true
                     | _ -> false
                     
                  let unvisited =
                     accR |> List.contains nv |> not
                     // && paths |> Seq.forall (fun p -> not (p |> List.contains nv))
                     
                  pathable && unvisited)
               |> Seq.map (fun (_, nv, _) -> nv)
               |> List.ofSeq
               
            gather (path :: paths) [] (queueThese @ remaining)

         else
            let next =
               neighbors
               |> Seq.filter (fun (nd, nv, nt) ->
                  let pathable =
                     match nt with
                     | Path -> true
                     | Slope sd when sd = nd -> true
                     | _ -> false
                     
                  let unvisited =
                     accR |> List.contains nv |> not &&
                     paths |> Seq.forall (fun p -> not (p |> List.contains nv))
                     
                  pathable && unvisited)
               |> Seq.map (fun (_, nv, _) -> nv)
               |> Seq.tryExactlyOne

            let newRemaining =
               match next with
               | Some n -> n :: remaining
               | None -> remaining
            
            gather paths (v :: accR) newRemaining

   gather [] [] [ start2 ]
   |> Array.ofSeq

let grid =
   data
   |> Seq.collecti (fun r ->
      Seq.choosei (fun c ch ->
         let v : Vector2i = { R = r; C = c }
         match ch with
         | '#' -> None
         | '.' -> Some (v, Path)
         | '^' -> Some (v, Slope N)
         | '<' -> Some (v, Slope W)
         | 'v' -> Some (v, Slope S)
         | '>' -> Some (v, Slope E)
         | _ -> raise invalidInput))
   |> Map.ofSeq

let getNeighbors (v : Vector2i) =
   [ N, { v with R = v.R - 1 }
     W, { v with C = v.C - 1 }
     S, { v with R = v.R + 1 }
     E, { v with C = v.C + 1 } ]
   |> List.filter (snd >> grid.ContainsKey)

let start, finish, junctions =
   let pathTiles =
      grid
      |> Map.toSeq
      |> Seq.filter (fun (_, tile) -> tile = Path)
      |> Array.ofSeq
  
   let start = pathTiles |> Seq.minBy (fun (v, _) -> v.R) |> fst
   let finish = pathTiles |> Seq.maxBy (fun (v, _) -> v.R) |> fst
   
   let junctions =
      pathTiles
      |> Seq.choose (fun pathTile ->
         let ns' = pathTile |> fst |> getNeighbors
         let ns = ns' |> Seq.map (fun (d, n) -> n, grid |> Map.find n)
         let allNeighborsSlope =
            ns
            |> Seq.forall (function
            | _, Slope _ -> true
            | _ -> false)
            
         if allNeighborsSlope then
            let outputs, inputs =
               ns'
               |> List.partition (fun (toNDir, n) ->
                  let nDir = grid |> Map.find n
                  match nDir with
                  | Slope d -> d = toNDir
                  | _ -> false)

            Some (pathTile, inputs, outputs)
         else
            None)
      |> List.ofSeq
   
   start, finish, junctions
   
let sources = start :: (junctions |> List.collect (fun (_, _, os) -> os |> List.map snd))
let sinks = finish :: (junctions |> List.map (fun ((j, _), _, _) -> j))
let subPaths =
   let rec tracePath accR tile =
      let thisTile = grid |> Map.find tile
      let next =
         tile
         |> getNeighbors
         |> Seq.filter (fun (d, n) ->
            let pathable =
               match thisTile, grid |> Map.find n with
               | Slope td, _ -> td = d
               | Path, Slope sd -> sd = d
               | Path, Path -> true
               | _ -> false
               
            pathable && not (accR |> List.contains n))
         |> Seq.exactlyOne
         |> snd
         
      if sinks |> List.contains next then
         (next :: tile :: accR) |> List.rev
      else
         tracePath (tile :: accR) next
      
   sources
   |> Seq.map (tracePath [])
   |> List.ofSeq

let connectedSubpaths2, connectedVerts =
   let xx =
      junctions
      |> List.collect (fun (p, is, os) ->
         let keys = is @ os |> List.map snd
         let (v, _) = p
         
         keys |> List.map (fun k -> k, v))
      |> Map.ofSeq
   
   let m1 =
      subPaths
      |> Seq.collect (fun sp ->
         let head = xx |> Map.tryFind sp.Head |> Option.defaultValue start
         let last = sp |> List.last
         seq {
            (head, last), sp.Length
            (last, head), sp.Length
         })
      |> Map.ofSeq
      
   let m2 =
      subPaths
      |> Seq.collect (fun sp ->
         let head = xx |> Map.tryFind sp.Head |> Option.defaultValue start
         let last = sp |> List.last
         seq {
            head, last
            last, head
         })
      |> Seq.groupBy fst
      |> Seq.map (mapSnd (Seq.map snd >> List.ofSeq))
      |> Map.ofSeq
   
   m1, m2

let part1 () =
   let startSubPath = subPaths |> List.find (fun sp -> sp.Head = start)
   
   let mutable longestPath = 0
   
   let rec find acc (cur : _ list) =
      let nexts =
         let nextHeads =
            cur
            |> List.last
            |> getNeighbors
            |> Seq.filter (fun (_, x) -> sources |> List.contains x)
            |> Seq.map snd
         
         nextHeads
         |> Seq.map (fun nh -> subPaths |> Seq.find (fun sp -> sp.Head = nh))
         |> List.ofSeq
      
      match nexts with
      | [] -> longestPath <- max longestPath (acc + cur.Length)
      | nexts ->
         for next in nexts do
            find (cur.Length + acc) next
   
   find 0 startSubPath
      
   longestPath - 1
      
let part2 () =
   
   let mutable longestPath = 0
   let mutable lp = None
   
   let rec find accR (cur : Vector2i) =
      if cur = finish then
         let path = cur :: accR
         let x = path |> Seq.pairwise |> Seq.sumBy (fun t -> connectedSubpaths2 |> Map.find t)
         if x > longestPath then
            longestPath <- max longestPath x
            lp <- Some path
      else
         let nexts =
            connectedVerts
            |> Map.find cur
            |> Seq.filter (fun sp -> not (accR |> List.contains sp))
            |> List.ofSeq

         for next in nexts do
            find (cur :: accR) next

   find [] start
      
   longestPath - 1

let part1Expected = 2042

let part2Expected = 6466
