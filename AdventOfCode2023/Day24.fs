/// https://adventofcode.com/2023/day/24
module AdventOfCode2023.Day24

open AdventOfCode.Common
open MathNet.Numerics.LinearAlgebra

let data = EmbeddedResource.loadText "Data/Day24.txt"

module Vector =
    let toTuple2 v = v |> Vector.toArray |> Array.toTuple2
    let toTuple3 v = v |> Vector.toArray |> Array.toTuple3

type Ray =
    { Origin: Vector<double>
      Velocity: Vector<double> }
    member this.PositionAt(t : double) = this.Origin + this.Velocity * t

let rays =
    let (|Vector|_|) str =
        match str |> String.split ", " with
        | [| Double x; Double y; Double z |] -> Some (vector [ x; y ; z ])
        | _ -> None
    
    let parseLine line =
        match line |> String.split " @ " with
        | [| Vector origin; Vector velocity |] ->  
            { Origin = origin
              Velocity = velocity }
        | _ -> raiseInvalidInput line 

    data
    |> Seq.map parseLine
    |> Seq.toArray

let intersection ray1 ray2 =
    let m =
        matrix [
            [ ray1.Velocity[0]; -ray2.Velocity[0] ]
            [ ray1.Velocity[1]; -ray2.Velocity[1] ]
        ]
    let b = (ray2.Origin - ray1.Origin)[0..1]
    let t, u = m.Solve(b) |> Vector.toTuple2

    if t >= 0.0 && u >= 0.0
    then Some (ray1.PositionAt(t))
    else None

let part1 () =
    let lo = 200_000_000_000_000.0
    let hi = 400_000_000_000_000.0
    let inRange = inRangeInclusive lo hi

    (rays, rays)
    ||> Seq.allPairs
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.choose (fun (a, b) -> intersection a b)
    |> Seq.filter (fun v -> inRange v[0] && inRange v[1])
    |> Seq.length
    |> flip (/) 2

let part2 () =
    let a, b, c = rays |> Array.truncate 3 |> Array.toTuple3

    let x_ao, y_ao, z_ao = a.Origin |> Vector.toTuple3
    let x_av, y_av, z_av = a.Velocity |> Vector.toTuple3
    let x_bo, y_bo, z_bo = b.Origin |> Vector.toTuple3
    let x_bv, y_bv, z_bv = b.Velocity |> Vector.toTuple3
    let x_co, y_co, z_co = c.Origin |> Vector.toTuple3
    let x_cv, y_cv, z_cv = c.Velocity |> Vector.toTuple3

    let m =
        matrix [
            [ y_av - y_bv; x_bv - x_av; 0;           y_bo - y_ao; x_ao - x_bo; 0           ]
            [ y_av - y_cv; x_cv - x_av; 0;           y_co - y_ao; x_ao - x_co; 0           ]
            [ z_bv - z_av; 0;           x_av - x_bv; z_ao - z_bo; 0;           x_bo - x_ao ]
            [ z_cv - z_av; 0;           x_av - x_cv; z_ao - z_co; 0;           x_co - x_ao ]
            [ 0;           z_av - z_bv; y_bv - y_av; 0;           z_bo - z_ao; y_ao - y_bo ]
            [ 0;           z_av - z_cv; y_cv - y_av; 0;           z_co - z_ao; y_ao - y_co ]
        ]

    let b =
        vector [
            (y_bo * x_bv - x_bo * y_bv) - (y_ao * x_av - x_ao * y_av)
            (y_co * x_cv - x_co * y_cv) - (y_ao * x_av - x_ao * y_av)
            (x_bo * z_bv - z_bo * x_bv) - (x_ao * z_av - z_ao * x_av)
            (x_co * z_cv - z_co * x_cv) - (x_ao * z_av - z_ao * x_av)
            (z_bo * y_bv - y_bo * z_bv) - (z_ao * y_av - y_ao * z_av)
            (z_co * y_cv - y_co * z_cv) - (z_ao * y_av - y_ao * z_av)
        ]

    let initialPosition = m.Solve(b) |> Vector.toArray |> Array.map (round >> int64)

    initialPosition |> Array.truncate 3 |> Array.sum

let part1Expected = 12783

let part2Expected = 948485822969419L
