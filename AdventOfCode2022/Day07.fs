namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/7
module Day07 =

    let data = EmbeddedResource.loadText "Data/Day07.txt"

    type Command =
        | CDRoot
        | CDUp
        | CDDown of directory : string
        | LS
        
    type Output =
        | File of size : int64 * fileName : string
        | Directory of directoryName : string

    type TerminalLine =
        | Command of Command
        | Output of Output
    
    let (|CommandLine|_|) (line : string) =
        match line with
        | "$ cd /"  -> Some CDRoot
        | "$ cd .." -> Some CDUp
        | Regex @"\$ cd (.*)" [ directoryName ] -> Some (CDDown directoryName)
        | "$ ls" -> Some LS
        | _ -> None

    let (|OutputLine|_|) (line : string) =
        match line with
        | Regex "(\d*) (.*)" [ Int64 size; fileName ] -> Some (File (size, fileName))
        | Regex "dir (.*)" [ directoryName ] -> Some (Directory directoryName) 
        | _ -> None
        
    let terminalLines =
        data
        |> Seq.map (function
            | CommandLine commandLine -> Command commandLine
            | OutputLine outputLine -> Output outputLine
            | junk -> failwith $"unrecognized line '{junk}'")
        |> List.ofSeq

    let getDirectoryToContents (commandTokens : TerminalLine list) =

        let applyCommand (command : Command) (pathR : string list) =
            match command with
            | CDRoot -> []
            | CDDown directoryName -> directoryName :: pathR
            | CDUp -> match pathR with _ :: newPathR -> newPathR | [] -> failwith "already at root"
            | LS -> pathR // noop

        let applyOutput (output : Output) (pathR : string list) pathToContents =
            let existingSize, existingDirectories =
                pathToContents
                |> Map.tryFind pathR
                |> Option.defaultValue (0L, []) 

            let newContents =
                match output with
                | File (size, _) -> size + existingSize,  existingDirectories
                | Directory directoryName -> existingSize, (directoryName :: pathR) :: existingDirectories
                
            pathToContents
            |> Map.add pathR newContents

        (([], Map.empty), commandTokens)
        ||> Seq.fold (fun (pathR, pathToContents) terminalLine ->
            match terminalLine with
            | Command command ->
                pathR |> applyCommand command,
                pathToContents
            | Output fs ->
                pathR,
                pathToContents |> applyOutput fs pathR)
        |> snd

    let directoryToContents =
        let directoryToLocalContents = terminalLines |> getDirectoryToContents
    
        let rec getDirectoryTotalSize (dirNames : string list list) (sizeSoFar : int64) =
            match dirNames with
            | [] -> sizeSoFar
            | dirName :: remainingDirs ->
                let sizeHere, directoriesHere = directoryToLocalContents |> Map.find dirName
                getDirectoryTotalSize (directoriesHere @ remainingDirs) (sizeSoFar + sizeHere)

        directoryToLocalContents
        |> Map.map (fun k _ -> getDirectoryTotalSize [ k ] 0L)
    
    let part1 () =
        // Sum of total sizes of directories with total size < 100_000L 
        directoryToContents
        |> Map.values
        |> Seq.filter (fun v -> v < 100_000L)
        |> Seq.sum
        |> printfn "%d"

    let part2 () =
        let availableSpace = 70_000_000L
        let requiredSpace = 30_000_000L
        let usedSpace = directoryToContents |> Map.find [] // total size of root directory
        let freeSpace = availableSpace - usedSpace
        let minSpaceNeeded = requiredSpace - freeSpace
        
        // find directory with smallest size that's over minSpaceNeeded 
        directoryToContents
        |> Map.filter (fun k totalSize -> totalSize >= minSpaceNeeded)
        |> Map.toSeq
        |> Seq.minBy snd
        |> printfn "%A"
