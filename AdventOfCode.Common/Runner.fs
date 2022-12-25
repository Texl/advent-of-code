namespace AdventOfCode.Common

open Microsoft.FSharp.Core

module Runner =
   open System
   open System.Diagnostics
   open System.Reflection
   
   module private Internals =
      let maxDay = 30
      let maxPart = 5
      
      type DRun = delegate of unit -> unit

      type Part =
         { Day : int
           Part : int
           Run : DRun }

      type Day =
         { Day : int
           Prewarm : DRun option
           Parts : Part list }

      let tryGetPrewarm (t : Type) =
         t.GetMethod($"prewarm", BindingFlags.Public ||| BindingFlags.Static)
         |> Option.ofObj
         |> Option.map (fun methodInfo -> methodInfo.CreateDelegate<DRun>()) 
      
      type DGetResult = delegate of unit -> obj
      
      let tryGetPart (t : Type) (day : int) (part : int) : Part option =
         opt {
            let! run =
               opt {
                  let! methodInfo = t.GetMethod($"part{part}", BindingFlags.Public ||| BindingFlags.Static) |> Option.ofObj
                  
                  if methodInfo.ReturnType = typeof<Void> then
                     return methodInfo.CreateDelegate<DRun>()
                  else
                     return
                        opt {
                           let getResult () = methodInfo.Invoke((), [||])
                           
                           let getExpectedResult =
                              opt {
                                 let! propertyInfo = t.GetProperty($"part{part}Expected", BindingFlags.Public ||| BindingFlags.Static) |> Option.ofObj
                                 
                                 if propertyInfo.PropertyType = methodInfo.ReturnType then
                                    return fun () -> propertyInfo.GetValue(null)
                              } 

                           return
                              DRun(fun () ->
                                 let result = getResult ()

                                 getExpectedResult
                                 |> Option.iter (fun f ->
                                    let expectedResult = f ()
                                    if result.Equals(expectedResult) then
                                       Console.ForegroundColor <- ConsoleColor.Green
                                       printf "OK "
                                    else
                                       Console.ForegroundColor <- ConsoleColor.Red
                                       printf "X "
                                    Console.ResetColor())

                                 printfn $"%A{result}")
                        }
                        |> Option.defaultWith (fun () -> failwith $"couldn't create delegate for day {day} part {part}")
               }

            return            
               { Day = day
                 Part = part
                 Run = run }
         }

      let tryGetDay (assembly : Assembly) (day : int) : Day option =
         assembly.GetTypes()
         |> Seq.tryFind (fun t -> t.Name = $"Day%02d{day}" || t.Name = $"Day%d{day}")
         |> Option.map (fun t ->
            { Day = day
              Prewarm = tryGetPrewarm t
              Parts = [ 1 .. maxPart ] |> List.choose (tryGetPart t day) })

      let getDays (assembly : Assembly) =
         [ 1 .. maxDay ]
         |> List.choose (tryGetDay assembly)

      let runPrewarm day (prewarm : DRun) =
         printfn $"\nDay {day} prewarm"
         let stopwatch = Stopwatch.StartNew()
         prewarm.Invoke()
         printfn $"{stopwatch.ElapsedMilliseconds}ms"
      
      let runPart (part : Part) =
         printfn $"\nDay {part.Day}, Part {part.Part}"
         let stopwatch = Stopwatch.StartNew()
         part.Run.Invoke()
         printfn $"{stopwatch.ElapsedMilliseconds}ms"
      
      let runDay (day : Day) =
         day.Prewarm
         |> Option.iter (runPrewarm day.Day)
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
