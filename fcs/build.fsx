// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#I "packages/FAKE/tools"
#r "packages/FAKE/tools/FakeLib.dll"
open System
open System.IO
open Fake
open Fake.AppVeyor
open Fake.ReleaseNotesHelper

#if MONO
// prevent incorrect output encoding (e.g. https://github.com/fsharp/FAKE/issues/1196)
System.Console.OutputEncoding <- System.Text.Encoding.UTF8
CleanDir (__SOURCE_DIRECTORY__ + "/../artifacts/TestResults") 
File.WriteAllText(__SOURCE_DIRECTORY__ + "/../artifacts/TestResults/notestsyet.txt","No tests yet")
let isMono = true
#else
let isMono = false
#endif


let dotnetExePath = DotNetCli.InstallDotNetSDK "2.1.403"

let runDotnet workingDir args =
    let result =
        ExecProcess (fun info ->
            info.FileName <- dotnetExePath
            info.WorkingDirectory <- workingDir
            info.Arguments <- args) TimeSpan.MaxValue

    if result <> 0 then failwithf "dotnet %s failed" args

let assertExitCodeZero x = if x = 0 then () else failwithf "Command failed with exit code %i" x

let runCmdIn workDir (exe:string) = Printf.ksprintf (fun (args:string) ->
#if MONO
        let exe = exe.Replace("\\","/")
        let args = args.Replace("\\","/")
        printfn "[%s] mono %s %s" workDir exe args
        Shell.Exec("mono", sprintf "%s %s" exe args, workDir)
#else
        printfn "[%s] %s %s" workDir exe args
        Shell.Exec(exe, args, workDir)
#endif
        |> assertExitCodeZero
)

// --------------------------------------------------------------------------------------
// The rest of the code is standard F# build script
// --------------------------------------------------------------------------------------

let releaseDir = Path.Combine(__SOURCE_DIRECTORY__, "../artifacts/bin/fcs")

// Read release notes & version info from RELEASE_NOTES.md
let release = LoadReleaseNotes (__SOURCE_DIRECTORY__ + "/RELEASE_NOTES.md")
let isAppVeyorBuild = buildServer = BuildServer.AppVeyor
let isJenkinsBuild = buildServer = BuildServer.Jenkins
let isVersionTag (tag: string) = Version.TryParse tag |> fst
let hasRepoVersionTag = isAppVeyorBuild && AppVeyorEnvironment.RepoTag && isVersionTag AppVeyorEnvironment.RepoTagName
let assemblyVersion = if hasRepoVersionTag then AppVeyorEnvironment.RepoTagName else release.NugetVersion

let buildVersion =
    if hasRepoVersionTag then assemblyVersion
    else if isAppVeyorBuild then sprintf "%s-b%s" assemblyVersion AppVeyorEnvironment.BuildNumber
    else assemblyVersion

Target "Clean" (fun _ ->
    CleanDir releaseDir
)

Target "Restore" (fun _ ->
    // We assume a paket restore has already been run
    runDotnet __SOURCE_DIRECTORY__ "restore ../src/buildtools/buildtools.proj -v n"
    runDotnet __SOURCE_DIRECTORY__ "restore FSharp.Compiler.Service.sln -v n"
)

Target "BuildVersion" (fun _ ->
    Shell.Exec("appveyor", sprintf "UpdateBuild -Version \"%s\"" buildVersion) |> ignore
)

Target "BuildTools" (fun _ ->
    runDotnet __SOURCE_DIRECTORY__ "build ../src/buildtools/buildtools.proj -v n -c Proto"
)

Target "Build" (fun _ ->
    runDotnet __SOURCE_DIRECTORY__ "build ../src/buildtools/buildtools.proj -v n -c Proto"
    runDotnet __SOURCE_DIRECTORY__ "build FSharp.Compiler.Service.sln -v n -c release"
)

Target "Test" (fun _ ->
    // This project file is used for the netcoreapp2.0 tests to work out reference sets
    runDotnet __SOURCE_DIRECTORY__ "restore ../tests/projects/Sample_NETCoreSDK_FSharp_Library_netstandard2_0/Sample_NETCoreSDK_FSharp_Library_netstandard2_0.fsproj -v n"
    runDotnet __SOURCE_DIRECTORY__ "build ../tests/projects/Sample_NETCoreSDK_FSharp_Library_netstandard2_0/Sample_NETCoreSDK_FSharp_Library_netstandard2_0.fsproj -v n"

    // Now run the tests
    runDotnet __SOURCE_DIRECTORY__ "test FSharp.Compiler.Service.Tests/FSharp.Compiler.Service.Tests.fsproj --no-restore --no-build -v n -c release"
)

Target "NuGet" (fun _ ->
    runDotnet __SOURCE_DIRECTORY__ "pack FSharp.Compiler.Service.sln -v n -c release"
)

Target "CodeGen.Fable" (fun _ ->
    let outDir = __SOURCE_DIRECTORY__ + "/fcs-fable/codegen/"

    // run FCS codegen (except that fssrgen runs without .resx output to inline it)
    runDotnet outDir "run -- ../../../src/fsharp/FSComp.txt FSComp.fs"
    runDotnet outDir "run -- ../../../src/fsharp/fsi/FSIstrings.txt FSIstrings.fs"

    // Fable-specific (comment the #line directive as it is not supported)
    ["lex.fs"; "pplex.fs"; "illex.fs"; "ilpars.fs"; "pars.fs"; "pppars.fs"]
    |> Seq.map (fun fileName -> outDir + fileName)
    |> RegexReplaceInFilesWithEncoding @"(?<!/)# (?=\d)" "//# " Text.Encoding.UTF8

    // prevent stack overflows on large files (make lexer rules inline)
    let pattern = @"(?<=and )(?!inline )([a-zA-Z]+ )+ *\(lexbuf "
    ["lex.fs"; "pplex.fs"; "illex.fs"]
    |> Seq.map (fun fileName -> outDir + fileName)
    |> RegexReplaceInFilesWithEncoding pattern @"inline $0" Text.Encoding.UTF8
)

Target "GenerateDocsEn" (fun _ ->
    executeFSIWithArgs "docsrc/tools" "generate.fsx" [] [] |> ignore
)

Target "GenerateDocsJa" (fun _ ->
    executeFSIWithArgs "docsrc/tools" "generate.ja.fsx" [] [] |> ignore
)

Target "PublishNuGet" (fun _ ->
    Paket.Push (fun p ->
        let apikey =
            match getBuildParam "nuget-apikey" with
            | s when not (String.IsNullOrWhiteSpace s) -> s
            | _ -> getUserInput "Nuget API Key: "
        { p with
            ApiKey = apikey
            WorkingDir = releaseDir })
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "Start" DoNothing
Target "Release" DoNothing
Target "GenerateDocs" DoNothing
Target "TestAndNuGet" DoNothing

"Clean"
  ==> "BuildTools"
  ==> "CodeGen.Fable"

"Start"
  =?> ("BuildVersion", isAppVeyorBuild)
  ==> "Restore"
  ==> "Build"

"Build"
  ==> "Test"

"Build"
  ==> "NuGet"

"Test"
  ==> "TestAndNuGet"

"NuGet"
  ==> "TestAndNuGet"
  
"Build"
  ==> "NuGet"
  ==> "PublishNuGet"
  ==> "Release"

"Build"
  ==> "GenerateDocsEn"
  ==> "GenerateDocs"

"Build"
  ==> "GenerateDocsJa"
  ==> "GenerateDocs"

"GenerateDocs"
  ==> "Release"

RunTargetOrDefault "Build"
