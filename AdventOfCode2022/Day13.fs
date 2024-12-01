namespace AdventOfCode2022

open System
open AdventOfCode.Common

/// https://adventofcode.com/2022/day/13
module Day13 =
    let data = EmbeddedResource.loadText "Data/Day13.txt"
    
    type PacketData =
        | Integer of int
        | List of PacketData list
    
    type Packet = PacketData list
        
    let rec parseNumber (chars : char list) (accR : char list) =
        match chars with
        | x :: rest when "0123456789".Contains x -> parseNumber rest (x :: accR)
        | rest -> accR |> Seq.rev |> Array.ofSeq |> String |> int, rest // put back non-digit
                
    let rec parseList (chars : char list) (acc : PacketData list) : PacketData list * char list =
        match chars with
        | '[' :: rest -> let list, rest = parseList rest [] in parseList rest (List list :: acc)
        | ',' :: rest -> parseList rest acc
        | ']' :: rest -> List.rev acc, rest
        | x :: rest -> let number, rest = parseNumber rest [ x ] in parseList rest (Integer number :: acc)
        | [] -> acc, []

    let parsePacket (str : string) : PacketData =
        parseList (str |> List.ofSeq) [] |> fst |> Seq.exactlyOne
    
    let packets =
        data
        |> List.ofSeq
        |> Array.split (fun s -> s = "")
        |> Seq.map (List.ofSeq >> function
            | [ p1; p2 ] -> parsePacket p1, parsePacket p2
            | _ -> raise invalidInput)
            
    let rec compare (packet1 : PacketData) (packet2 : PacketData) =
        match packet1, packet2 with
        | Integer p1, Integer p2 -> p1.CompareTo(p2)
        | List (h1 :: t1), List (h2 :: t2) ->
            match compare h1 h2 with
            | 0 -> compare (List t1) (List t2)
            | comp -> comp
        | List [], List [] -> 0
        | List [], _ -> -1
        | _, List [] -> 1
        | Integer p1, List p2 -> compare (List [ Integer p1 ]) (List p2)
        | List p1, Integer p2 -> compare (List p1) (List [ Integer p2 ])

    let minPacket a b = if compare a b <= 0 then a else b

    let rec sortPackets packets acc =
        match packets with
        | [] -> acc |> List.rev
        | xs ->
            let next = xs |> List.reduce minPacket
            sortPackets (xs |> List.except [ next ]) (next :: acc)

    let part1 () =
        packets
        |> Seq.mapi (fun ord (packet1, packet2) ->
            match compare packet1 packet2 with
            | -1 -> Some (ord + 1)
            | _ -> None)
        |> Seq.choose id
        |> Seq.sum
        |> printfn "%d"
    
    let part2 () =
        let divider1 = List [ List [ Integer 2 ] ]
        let divider2 = List [ List [ Integer 6 ] ]
        
        let allPackets =
            [ yield! packets |> Seq.collect (fun (a, b) -> [ a; b ])
              divider1
              divider2 ]
        
        let sorted = sortPackets allPackets []
        
        let divider1Index = sorted |> List.findIndex (fun p -> p = divider1)
        let divider2Index = sorted |> List.findIndex (fun p -> p = divider2)
        
        (divider1Index + 1) * (divider2Index + 1)
        |> printfn "%d"
