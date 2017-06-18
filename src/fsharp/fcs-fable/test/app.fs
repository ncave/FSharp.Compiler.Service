module App
open Microsoft.FSharp.Compiler.SourceCodeServices

[<EntryPoint>]
let main argv =
    printfn "Parsing begins..."

    let metadataPath = "/temp/metadata/"
#if DOTNETCORE
    let references =
      [ "FSharp.Core"
        "mscorlib"
        "System.Collections"
        "System.Console"
        "System.Diagnostics.Debug"
        "System.Globalization"
        "System.IO"
        "System.Linq"
        "System.Linq.Expressions"
        "System.Net.Requests"
        "System.ObjectModel"
        "System.Reflection"
        "System.Reflection.Primitives"
        "System.Resources.ResourceManager"
        "System.Runtime"
        "System.Runtime.InteropServices"
        "System.Runtime.Numerics"
        "System.Threading"
        "System.Threading.Tasks"
        "System.Xml.XDocument"
    ]
#else
    let references = ["FSharp.Core";"mscorlib";"System";"System.Core";"System.Data";"System.IO";"System.Xml";"System.Numerics";"Fable.Core"]
#endif

#if DOTNETCORE || DOTNET40
    let readAllBytes = fun (fileName:string) -> System.IO.File.ReadAllBytes (metadataPath + fileName)
    let readAllText = fun (filePath:string) -> System.IO.File.ReadAllText (filePath, System.Text.Encoding.UTF8)
#else
    let readFileSync: System.Func<string, byte[]> = Fable.Core.JsInterop.import "readFileSync" "fs"
    let readTextSync: System.Func<string, string, string> = Fable.Core.JsInterop.import "readFileSync" "fs"

    let readAllBytes = fun (fileName:string) -> readFileSync.Invoke (metadataPath + fileName)
    let readAllText = fun (filePath:string) -> readTextSync.Invoke (filePath, "utf8")
#endif

    let fileName = "test_script.fsx"
    let source = readAllText fileName

    let checker = InteractiveChecker.Create(references, readAllBytes)
    // let parseResults = checker.ParseScript(fileName,source)
    let parseResults, typeCheckResults, projectResults = checker.ParseAndCheckScript(fileName,source)
    
    printfn "parseResults.ParseHadErrors: %A" parseResults.ParseHadErrors
    printfn "parseResults.Errors: %A" parseResults.Errors
    //printfn "parseResults.ParseTree: %A" parseResults.ParseTree
    
    printfn "typeCheckResults Errors: %A" typeCheckResults.Errors
    printfn "typeCheckResults Entities: %A" typeCheckResults.PartialAssemblySignature.Entities
    //printfn "typeCheckResults Attributes: %A" typeCheckResults.PartialAssemblySignature.Attributes

    printfn "projectResults Errors: %A" projectResults.Errors
    //printfn "projectResults Contents: %A" projectResults.AssemblyContents

    printfn "Typed AST:"
    projectResults.AssemblyContents.ImplementationFiles
    |> Seq.iter (fun file -> AstPrint.printFSharpDecls "" file.Declarations |> Seq.iter (printfn "%s"))

    let inputLines = source.Split('\n')
    async {
        // Get tool tip at the specified location
        let! tip = typeCheckResults.GetToolTipTextAlternate(3, 7, inputLines.[2], ["foo"], FSharpTokenTag.IDENT)
        (sprintf "%A" tip).Replace("\n","") |> printfn "---> ToolTip Text = %A" // should be "FSharpToolTipText [...]"
    } |> Async.StartImmediate
    async {
        // Get declarations (autocomplete) for a location
        let! decls = typeCheckResults.GetDeclarationListInfo(Some parseResults, 6, 25, inputLines.[5], [], "msg", (fun _ -> []), fun _ -> false)
        [ for item in decls.Items -> item.Name ] |> printfn "---> AutoComplete = %A" // should be string methods
    } |> Async.StartImmediate

    0
