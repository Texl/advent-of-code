namespace AdventOfCode.Common

module Runner =
   open System.Diagnostics
   open System.Reflection
   
   module private Internals =
      open System

      let maxDay = 30
      let maxPart = 5
      
      type DRun = delegate of unit -> unit
      
      type Part =
         { Day : int
           Part : int
           Run : DRun }

      type Day =
         { Day : int
           Parts : Part list }

      let tryGetPart (t : Type) (day : int) (part : int) : Part option =
         t.GetMethod($"part{part}", BindingFlags.Public ||| BindingFlags.Static)
         |> Option.ofObj
         |> Option.map (fun methodInfo ->
            { Day = day
              Part = part
              Run = methodInfo.CreateDelegate<DRun>() }) 
         
      let tryGetDay (assembly : Assembly) (day : int) : Day option =
         assembly.GetTypes()
         |> Seq.tryFind (fun t -> t.Name = $"Day%02d{day}" || t.Name = $"Day%d{day}")
         |> Option.map (fun t ->
            { Day = day
              Parts = [ 1 .. maxPart ] |> List.choose (tryGetPart t day) })

      let getDays (assembly : Assembly) =
         [ 1 .. maxDay ]
         |> List.choose (tryGetDay assembly)

      let runPart (part : Part) =
         printfn $"\nDay {part.Day}, Part {part.Part}"
         let stopwatch = Stopwatch.StartNew()
         part.Run.Invoke()
         printfn $"{stopwatch.ElapsedMilliseconds}ms"
      
      let runDay (day : Day) =
         day.Parts
         |> List.iter runPart
   
   let runDay n =
      Assembly.GetCallingAssembly()
      |> Internals.getDays
      |> List.tryFind (fun d -> d.Day = n)
      |> Option.iter Internals.runDay

   let runDayPart n m =
      Assembly.GetCallingAssembly()
      |> Internals.getDays
      |> List.tryFind (fun d -> d.Day = n)
      |> Option.bind (fun day ->
         day.Parts
         |> List.tryFind (fun p -> p.Part = m))
      |> Option.iter Internals.runPart

   let runAll () =
      Assembly.GetCallingAssembly()
      |> Internals.getDays
      |> List.iter (fun day ->
         day |> Internals.runDay
         printfn "\n----------------------------------------------------------------")

   let runLatest () =
      Assembly.GetCallingAssembly()
      |> Internals.getDays
      |> List.last
      |> Internals.runDay
