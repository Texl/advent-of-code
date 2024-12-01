namespace AdventOfCode.Common

module EmbeddedResource =

    open System
    open System.IO
    open System.Reflection
    
    /// Load a text resource embedded in the calling assembly specifed by the relative path from the assembly's project root. 
    let loadText (relativeResourcePath : string) : seq<string> =

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
