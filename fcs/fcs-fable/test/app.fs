module App
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

[<EntryPoint>]
let main argv =
    printfn "Parsing begins..."

#if USE_DOTNET40_BINARIES
    let metadataPath = "/temp/metadata/"
    let references = ["Fable.Core";"FSharp.Core";"mscorlib";"System";"System.Core";"System.Data";"System.IO";"System.Xml";"System.Numerics"]
#else // dotnet core 2.0 binaries
    let metadataPath = "/temp/metadata2/"
    let references = [
        "Fable.Core"
        "FSharp.Core"
        "Microsoft.CSharp"
        "Microsoft.VisualBasic"
        "Microsoft.Win32.Primitives"
        "mscorlib"
        "netstandard"
        "System.AppContext"
        "System.Buffers"
        "System.Collections.Concurrent"
        "System.Collections"
        "System.Collections.Immutable"
        "System.Collections.NonGeneric"
        "System.Collections.Specialized"
        "System.ComponentModel.Annotations"
        "System.ComponentModel.Composition"
        "System.ComponentModel.DataAnnotations"
        "System.ComponentModel"
        "System.ComponentModel.EventBasedAsync"
        "System.ComponentModel.Primitives"
        "System.ComponentModel.TypeConverter"
        "System.Configuration"
        "System.Console"
        "System.Core"
        "System.Data.Common"
        "System.Data"
        "System.Diagnostics.Contracts"
        "System.Diagnostics.Debug"
        "System.Diagnostics.DiagnosticSource"
        "System.Diagnostics.FileVersionInfo"
        "System.Diagnostics.Process"
        "System.Diagnostics.StackTrace"
        "System.Diagnostics.TextWriterTraceListener"
        "System.Diagnostics.Tools"
        "System.Diagnostics.TraceSource"
        "System.Diagnostics.Tracing"
        "System"
        "System.Drawing"
        "System.Drawing.Primitives"
        "System.Dynamic.Runtime"
        "System.Globalization.Calendars"
        "System.Globalization"
        "System.Globalization.Extensions"
        "System.IO.Compression"
        "System.IO.Compression.FileSystem"
        "System.IO.Compression.ZipFile"
        "System.IO"
        "System.IO.FileSystem"
        "System.IO.FileSystem.DriveInfo"
        "System.IO.FileSystem.Primitives"
        "System.IO.FileSystem.Watcher"
        "System.IO.IsolatedStorage"
        "System.IO.MemoryMappedFiles"
        "System.IO.Pipes"
        "System.IO.UnmanagedMemoryStream"
        "System.Linq"
        "System.Linq.Expressions"
        "System.Linq.Parallel"
        "System.Linq.Queryable"
        "System.Net"
        "System.Net.Http"
        "System.Net.HttpListener"
        "System.Net.Mail"
        "System.Net.NameResolution"
        "System.Net.NetworkInformation"
        "System.Net.Ping"
        "System.Net.Primitives"
        "System.Net.Requests"
        "System.Net.Security"
        "System.Net.ServicePoint"
        "System.Net.Sockets"
        "System.Net.WebClient"
        "System.Net.WebHeaderCollection"
        "System.Net.WebProxy"
        "System.Net.WebSockets.Client"
        "System.Net.WebSockets"
        "System.Numerics"
        "System.Numerics.Vectors"
        "System.ObjectModel"
        "System.Reflection.DispatchProxy"
        "System.Reflection"
        "System.Reflection.Emit"
        "System.Reflection.Emit.ILGeneration"
        "System.Reflection.Emit.Lightweight"
        "System.Reflection.Extensions"
        "System.Reflection.Metadata"
        "System.Reflection.Primitives"
        "System.Reflection.TypeExtensions"
        "System.Resources.Reader"
        "System.Resources.ResourceManager"
        "System.Resources.Writer"
        "System.Runtime.CompilerServices.VisualC"
        "System.Runtime"
        "System.Runtime.Extensions"
        "System.Runtime.Handles"
        "System.Runtime.InteropServices"
        "System.Runtime.InteropServices.RuntimeInformation"
        "System.Runtime.InteropServices.WindowsRuntime"
        "System.Runtime.Loader"
        "System.Runtime.Numerics"
        "System.Runtime.Serialization"
        "System.Runtime.Serialization.Formatters"
        "System.Runtime.Serialization.Json"
        "System.Runtime.Serialization.Primitives"
        "System.Runtime.Serialization.Xml"
        "System.Security.Claims"
        "System.Security.Cryptography.Algorithms"
        "System.Security.Cryptography.Csp"
        "System.Security.Cryptography.Encoding"
        "System.Security.Cryptography.Primitives"
        "System.Security.Cryptography.X509Certificates"
        "System.Security"
        "System.Security.Principal"
        "System.Security.SecureString"
        "System.ServiceModel.Web"
        "System.ServiceProcess"
        "System.Text.Encoding"
        "System.Text.Encoding.Extensions"
        "System.Text.RegularExpressions"
        "System.Threading"
        "System.Threading.Overlapped"
        "System.Threading.Tasks.Dataflow"
        "System.Threading.Tasks"
        "System.Threading.Tasks.Extensions"
        "System.Threading.Tasks.Parallel"
        "System.Threading.Thread"
        "System.Threading.ThreadPool"
        "System.Threading.Timer"
        "System.Transactions"
        "System.Transactions.Local"
        "System.ValueTuple"
        "System.Web"
        "System.Web.HttpUtility"
        "System.Windows"
        "System.Xml"
        "System.Xml.Linq"
        "System.Xml.ReaderWriter"
        "System.Xml.Serialization"
        "System.Xml.XDocument"
        "System.Xml.XmlDocument"
        "System.Xml.XmlSerializer"
        "System.Xml.XPath"
        "System.Xml.XPath.XDocument"
        "WindowsBase"
    ]
#endif

#if DOTNET_FILE_SYSTEM
    let readAllBytes = fun (fileName:string) -> System.IO.File.ReadAllBytes (metadataPath + fileName)
    let readAllText = fun (filePath:string) -> System.IO.File.ReadAllText (filePath, System.Text.Encoding.UTF8)
#else
    let readFileSync: System.Func<string, byte[]> = Fable.Core.JsInterop.import "readFileSync" "fs"
    let readTextSync: System.Func<string, string, string> = Fable.Core.JsInterop.import "readFileSync" "fs"

    let readAllBytes = fun (fileName:string) -> readFileSync.Invoke (metadataPath + fileName)
    let readAllText = fun (filePath:string) -> readTextSync.Invoke (filePath, "utf8")
#endif

    let checker = InteractiveChecker.Create(references, readAllBytes)

    let fileName = "test_script.fsx"
    let source = readAllText fileName

    // let parseResults = checker.ParseScript(fileName,source)
    let parseResults, typeCheckResults, projectResults = checker.ParseAndCheckScript(fileName, source)
    
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
        let! tip = typeCheckResults.GetToolTipText(3, 7, inputLines.[2], ["foo"], FSharpTokenTag.IDENT)
        (sprintf "%A" tip).Replace("\n","") |> printfn "---> ToolTip Text = %A" // should be "FSharpToolTipText [...]"
    } |> Async.StartImmediate
    async {
        // Get declarations (autocomplete) for a location
        let partialName = { QualifyingIdents = []; PartialIdent = "msg"; EndColumn = 25; LastDotPos = None }
        let! decls = typeCheckResults.GetDeclarationListInfo(Some parseResults, 6, inputLines.[5], partialName, (fun _ -> []), fun _ -> false)
        [ for item in decls.Items -> item.Name ] |> printfn "---> AutoComplete = %A" // should be string methods
    } |> Async.StartImmediate

    0
