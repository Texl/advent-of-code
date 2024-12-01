/// https://adventofcode.com/2023/day/25
module AdventOfCode2023.Day25

open AdventOfCode.Common
open System.Collections.Generic

let testData =
   [
      "jqt: rhn xhk nvd"
      "rsh: frs pzl lsr"
      "xhk: hfx"
      "cmg: qnr nvd lhk bvb"
      "rhn: xhk bvb hfx"
      "bvb: xhk hfx"
      "pzl: lsr hfx nvd"
      "qnr: nvd"
      "ntq: jqt hfx bvb xhk"
      "nvd: lhk"
      "lsr: lhk"
      "rzs: qnr cmg lsr rsh"
      "frs: qnr lhk lsr"
   ]

let testData2 =
   [
      "a: b d e"
      "b: a c"
      "c: b d"
      "d: a c e"
      "e: a d"
   ]

let data = EmbeddedResource.loadText "Data/Day25.txt"

type Edge =
   { Original : Set<string>
     Current : Set<string> }

type Node =
   { Name : string
     Count : int }

let edges =
   data
   |> Seq.collect (function
      | Regex "(\w+): (.*)" [ leftNode; rightNodes ] ->
         rightNodes
         |> String.split " "
         |> Seq.map (fun rightNode ->
            let nodes = set [ leftNode; rightNode ]
            { Original = nodes
              Current = nodes })
      | x -> raiseInvalidInput x)
   |> Array.ofSeq

let nodes =
   edges
   |> Seq.collect (fun edge -> edge.Current |> Set.toSeq)
   |> Seq.distinct
   |> Seq.map (fun node ->
      { Name = node
        Count = 1 })
   |> Set.ofSeq

let seedRng = System.Random()

let contract skipme (sourceEdges : Edge[], sourceNodes : Set<Node>) =
   let seed = seedRng.Next()
   let rng = System.Random(seed)
   let random n = rng.Next(n)
   printfn $"seed: {seed}"

   let mutable edges = List(sourceEdges)
   let mutable nodes = List(sourceNodes)
   while nodes.Count > 2 do
      // print progress
      // if nodes.Count % 100 = 0 then
      //    printfn $"{edges.Count} edges, {nodes.Count} nodes"

      let randomEdge = edges[(random edges.Count)]
      
      if skipme |> Set.contains randomEdge.Original |> not then 
         let nodesToRemove = nodes.FindAll(fun node -> randomEdge.Current |> Set.contains node.Name)

         let newNode =
            { Name = $"{random 0x7FFFFFFF}"
              Count = nodesToRemove |> Seq.sumBy (fun node -> node.Count) }

         for toRemove in nodesToRemove do
            nodes.Remove(toRemove) |> ignore

         nodes.Add(newNode)

         let edgesToRemove = edges.FindAll(fun edge -> edge.Current |> Set.intersect randomEdge.Current |> Set.isEmpty |> not)
         
         for edgeToRemove in edgesToRemove do
            edges.Remove(edgeToRemove) |> ignore

            let current =
               edgeToRemove.Current
               |> Set.filter (fun node -> not (randomEdge.Current |> Set.contains node))
               |> Set.add newNode.Name
               
            if current.Count > 1 then
               edges.Add({ edgeToRemove with Current = current })

   edges, nodes

let print (edges : Edge[], nodes : Set<Node>) =
   printfn "state"
   printfn "\tedges"
   for edge in edges do
      printfn $"\t\t{edge.Current} / {edge.Original}"
   printfn "\tnodes"
   for node in nodes do
      printfn $"\t\t{node.Name} / {node.Count}"

let writeGraphviz () =
   data
   |> Seq.iter (function
      | Regex "(\w+): (.*)" [ leftNode; rightNodes ] ->
         let rightNodesStr = rightNodes |> String.split " " |> String.concat " & "
         printfn $"{leftNode} -> {rightNodesStr}"
      | x -> raiseInvalidInput x)

let solve () =
   // edges discovered by hand / inspection of graph rendered via Graphviz
   let skipme =
      set [
         set [ "vph"; "tjz" ]
         set [ "jhq"; "zkt" ]
         set [ "pgt"; "lnr" ]
      ]
   
   let graph = (edges, nodes)
   
   let rec attempt n =
      printfn $"attempt {n}..."
      let edges, nodes = contract skipme graph
      let finalState = edges |> Array.ofSeq, nodes |> Set.ofSeq
      print finalState
      
      if edges.Count <> 3 then
         attempt (n + 1)
      else
         finalState
   
   match attempt 0 |> mapSnd Set.toArray with
   | _, [| n0; n1 |] -> n0.Count * n1.Count
   | _ -> raise unreachable

       
let part1 () =
   // writeGraphviz ()
   solve ()

let part1Expected = 547410
