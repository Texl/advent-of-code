namespace AdventOfCode.Common

module FsTypes =
    type u8 = uint8
    type u16 = uint16
    type u32 = uint
    type u64 = uint64
    type i8 = int8
    type i16 = int16
    type i32 = int32
    type i64 = int64
    type f32 = float
    type f64 = double
    type d128 = decimal

module TryParse =
    open FsTypes

    let private tryParseWith f (s : string) =
        match f s with
        | true, v -> Some v
        | _ -> None

    let u8 s = tryParseWith u8.TryParse s
    let u16 s = tryParseWith u16.TryParse s
    let u32 s = tryParseWith u32.TryParse s
    let u64 s = tryParseWith u64.TryParse s
    let i8 s = tryParseWith i8.TryParse s
    let i16 s = tryParseWith i16.TryParse s
    let i32 s = tryParseWith i32.TryParse s
    let i64 s = tryParseWith i64.TryParse s
    let f32 s = tryParseWith f64.TryParse s
    let f64 s = tryParseWith f64.TryParse s
    let d128 s = tryParseWith d128.TryParse s


[<AutoOpen>]
module Parse =
    open System.Text.RegularExpressions
    open FsTypes

    let (|U8|_|) (s : string) : u8 option = TryParse.u8 s
    let (|U16|_|) (s : string) : u16 option = TryParse.u16 s
    let (|U32|_|) (s : string) : u32 option = TryParse.u32 s
    let (|U64|_|) (s : string) : u64 option = TryParse.u64 s
    let (|I8|_|) (s : string) : i8 option = TryParse.i8 s
    let (|I16|_|) (s : string) : i16 option = TryParse.i16 s
    let (|I32|_|) (s : string) : i32 option = TryParse.i32 s
    let (|I64|_|) (s : string) : i64 option = TryParse.i64 s
    let (|F32|_|) (s : string) : f32 option = TryParse.f32 s
    let (|F64|_|) (s : string) : f64 option = TryParse.f64 s
    let (|D128|_|) (s : string) : d128 option = TryParse.d128 s
       
    let (|Regex|_|) pattern s =
        let m = Regex.Match(s, pattern)
        if m.Success
        then Some (m.Groups |> Seq.map (fun g -> g.Value) |> Seq.tail |> List.ofSeq)
        else None
        
