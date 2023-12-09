/// https://adventofcode.com/2023/day/5
module AdventOfCode2023.Day05

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day05.txt"

let parseSeeds lines =
   match lines |> Seq.exactlyOne |> Regex.tryMatch "seeds: ((?<Seed>\d+)\s*)+" with
   | Some m -> m["Seed"] |> Seq.map (fun c -> c.Value |> int64) |> List.ofSeq
   | None -> failwith "seeds parse"

let tryMap a b l value =
   if value >= a && value < a + l
   then Some (b + value - a)
   else None

let parseMapping lines =
   lines
   |> List.skip 1
   |> List.map (function
      | Regex "(\d+)\s*(\d+)\s*(\d+)" [ Int64 dst; Int64 src; Int64 len  ] -> dst, src, len
      | _ -> raise invalidInput)

let seeds, mappings =
   data
   |> List.split String.isEmpty
   |> function
      | seedChunk :: mappingChunks ->
         seedChunk |> parseSeeds,
         mappingChunks |> List.map parseMapping
      | _ -> raise invalidInput

let mapFwd =
   mappings
   |> List.map (fun mapping ->
      mapping 
      |> List.map (fun (dst, src, len) -> tryMap src dst len)
      |> List.reduce (fun f g input -> f input |> Option.orElseWith (fun () -> g input))
      >> (flip Option.defaultValue)
      |> w)
   |> Seq.reduce (>>)

let mapRev =  
   mappings
   |> List.map (fun mapping ->
      mapping 
      |> List.map (fun (dst, src, len) -> tryMap dst src len)
      |> List.reduce (fun f g input -> f input |> Option.orElseWith (fun () -> g input))
      >> (flip Option.defaultValue)
      |> w)
   |> Seq.reduce (<<)
   
let part1 () =
   seeds
   |> Seq.map mapFwd
   |> Seq.min

let part2 () =
   let seedRangeTests =
      seeds
      |> List.chunkBySize 2
      |> List.map (function
         | [ source; length ] -> fun value -> value >= source && value < source + length
         | _ -> raise invalidInput)

   let isSeed value =
      seedRangeTests
      |> Seq.exists (fun test -> test value)
      
   Seq.initInfinite int64
   |> Seq.find (mapRev >> isSeed)

let part1Expected = 457535844L

let part2Expected = 41222968L
