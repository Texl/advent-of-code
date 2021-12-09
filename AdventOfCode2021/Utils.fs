namespace AdventOfCode2021

module Utils =

    open System
    open System.IO
    open System.Reflection
    
    /// Load a text resource embedded in the calling assembly specifed by the relative path from the assembly's project root. 
    let loadTextResource (relativeResourcePath : string) : seq<string> =

        // Resource names replace directory separators with periods
        let resourceNameEnd = relativeResourcePath.Replace('\\', '.').Replace('/', '.')

        let callingAssembly = Assembly.GetCallingAssembly()

        let manifestResourceNames = callingAssembly.GetManifestResourceNames()
        
        let resourceName =
            manifestResourceNames
            |> Seq.filter (fun resourceName -> resourceName.EndsWith resourceNameEnd)
            |> Seq.sortBy (fun resourceName -> resourceName.Length) // The shortest name is the most explicitly specified
            |> Seq.tryHead
            |> Option.defaultWith (fun () -> raise (ArgumentException($"Couldn't match resource name \"{resourceNameEnd}\" with any of: %A{manifestResourceNames}")))
        
        seq {
            let stream = callingAssembly.GetManifestResourceStream(resourceName)
            use reader = new StreamReader(stream)
            while not reader.EndOfStream do
                yield reader.ReadLine()
        }


// I realized on day 4 that List.transpose already exists - leaving this here for posterity. Knowing how to write it proved helpful in solving day 3, at least...  
//module List =
//
//        /// Transpose a list of lists - requires all lists to be of the same length.
//    let rec transpose (lists : 'a list list) =
//        match lists with
//        | (_ :: _) :: _ as m -> List.map List.head m :: transpose (List.map List.tail m)
//        | _ -> []


module Seq =

    // Not optimized, but meh
    let collecti f = Seq.indexed >> Seq.collect (fun (ord, x) -> f ord x)
