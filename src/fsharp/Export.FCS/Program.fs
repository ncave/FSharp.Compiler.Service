open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

let checker = FSharpChecker.Create()

let parseAndCheckScript (file, input) = 
    let projectOptions = checker.GetProjectOptionsFromScript(file, input) |> Async.RunSynchronously
    let parseResult, typedRes = checker.ParseAndCheckFileInProject(file, 0, input, projectOptions) |> Async.RunSynchronously
    
    if parseResult.Errors.Length > 0 then
        printfn "---> Parse Input = %A" input
        printfn "---> Parse Error = %A" parseResult.Errors

    match typedRes with
    | FSharpCheckFileAnswer.Succeeded(res) -> parseResult, res
    | res -> failwithf "Parsing did not finish... (%A)" res

[<EntryPoint>]
let main argv = 
    ignore argv
    printfn "Exporting metadata..."
    let file = "/Test.fsx"
    let input =
#if DOTNETCORE
        """
        #r "System.Private.CoreLib.dll"
        #r "System.Threading.dll"
        #r "System.Globalization.dll"
        #r "System.Reflection.Primitives.dll"
        #r "System.Resources.ResourceManager.dll"
        """ +
#endif
        """
        printfn "answer=%A" 42
        """
    // parse script just to export metadata
    parseAndCheckScript(file, input) |> ignore
    0
