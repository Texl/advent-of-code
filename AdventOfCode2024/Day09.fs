/// https://adventofcode.com/2024/day/9
module AdventOfCode2024.Day09

open AdventOfCode.Common

let test = false

let testData = seq { "2333133121414131402" }

let data = if test then testData else EmbeddedResource.loadText "Data/Day09.txt"

type Block =
   { FileIndex : int option
     StartIndex : int
     Length : int }

let blocks =
   let sizes =
      data
      |> Seq.exactlyOne
      |> Seq.map (string >> int)
   
   (((true, 0, 0), []), sizes)
   ||> Seq.fold (fun ((isFile, fileIndex, startIndex), blocksR) size ->
      let nextIsFile = not isFile
      let nextFileIndex = if isFile then fileIndex + 1 else fileIndex
      let nextStartIndex = startIndex + size
      let block =
         { FileIndex = if isFile then Some fileIndex else None
           StartIndex = startIndex
           Length = size }
      (nextIsFile, nextFileIndex, nextStartIndex), block :: blocksR)
   |> snd
   |> List.rev
   
let part1 () =
   let expanded =
      blocks
      |> Seq.collect (fun block -> Seq.init block.Length (fun _ -> block.FileIndex))
      |> List.ofSeq

   let totalFree =
      blocks |> Seq.sumBy (fun block -> if block.FileIndex.IsSome then 0 else block.Length)

   let indexedFree =
      seq {
         yield! expanded
         yield! Seq.initInfinite (fun _ -> None)
      }
      |> Seq.indexed
      |> Seq.choose (fun (ord, value) ->
         match value with
         | None -> Some ord
         | _ -> None)
      |> Seq.truncate totalFree
      |> List.ofSeq
      
   let totalFileLength =
      blocks |> Seq.sumBy (fun block -> if block.FileIndex.IsSome then block.Length else 0)

   let frees =
      expanded |> Seq.truncate totalFileLength |> Seq.filter Option.isNone |> Seq.length 
      
   let indexFileMoves =
      (indexedFree, expanded |> List.rev |> Seq.choose id) ||> Seq.zip |> Seq.truncate frees |> List.ofSeq

   let indexFiles = expanded |> List.indexed |> List.truncate totalFileLength |> List.choose (fun (ord, ob) -> ob |> Option.map (fun b -> ord, b))

   (indexFileMoves |> Seq.sumBy (fun (ord, b) -> int64 ord * int64 b)) +   
   (indexFiles |> Seq.sumBy (fun (ord, b) -> int64 ord * int64 b))

let part2 () =
   let blockList = ResizeArray(blocks)
   
   let maxFileIndex = blocks |> Seq.choose _.FileIndex |> Seq.max
   
   for fileIndex = maxFileIndex downto 0 do
      let fileBlockIndex =
         blockList.FindIndex(fun block -> block.FileIndex = Some fileIndex)

      let fileBlock = blockList[fileBlockIndex]

      let maybeFreeBlockIndex =
         blockList.FindIndex(fun freeBlock ->
            freeBlock.FileIndex.IsNone &&
            freeBlock.Length >= fileBlock.Length &&
            freeBlock.StartIndex < fileBlock.StartIndex)
         |> function
            | -1 -> None
            | index -> Some index
            
      match maybeFreeBlockIndex with
      | Some freeBlockIndex ->
         let freeBlock = blockList[freeBlockIndex]
         let newFileBlock = { fileBlock with StartIndex = freeBlockIndex }
         let newFreeBlock = { freeBlock with StartIndex = freeBlock.StartIndex + fileBlock.Length; Length = freeBlock.Length - fileBlock.Length }
         blockList.RemoveAt fileBlockIndex
         blockList.Insert(fileBlockIndex, { fileBlock with FileIndex = None })
         blockList.RemoveAt freeBlockIndex
         blockList.Insert(freeBlockIndex, newFreeBlock)
         blockList.Insert(freeBlockIndex, newFileBlock)
      | None -> ()

   blockList
   |> Seq.collect (fun block -> Seq.init block.Length (fun _ -> block.FileIndex))
   |> Seq.indexed
   |> Seq.choose (fun (ord, fileIndex) -> fileIndex |> Option.map ((*) ord))
   |> Seq.sum

let part1Expected = 6430446922192L

let part2Expected = 6460170593016L
