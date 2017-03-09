// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

// Open up the compiler as an incremental service for parsing,
// type checking and intellisense-like environment-reporting.

namespace Microsoft.FSharp.Compiler.SourceCodeServices

#nowarn "1182"

open Internal.Utilities
open Internal.Utilities.Collections
open Microsoft.FSharp.Collections

open System
open System.Text
open System.Threading
open System.Collections.Generic

open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics 
open Microsoft.FSharp.Compiler.AbstractIL.Internal  
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library  

open Microsoft.FSharp.Compiler.AccessibilityLogic
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.CompileOps
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.ReferenceResolver
open Microsoft.FSharp.Compiler.PrettyNaming
open Microsoft.FSharp.Compiler.Parser
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Lexhelp
open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Tastops.DebugPrint
open Microsoft.FSharp.Compiler.TcGlobals 
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.InfoReader
open Microsoft.FSharp.Compiler.NameResolution
open Microsoft.FSharp.Compiler.TypeChecker
open Microsoft.FSharp.Compiler.SourceCodeServices.ItemDescriptionsImpl 


//-------------------------------------------------------------------------
// InteractiveChecker
//-------------------------------------------------------------------------

type InteractiveChecker(references: string list, readAllBytes: string -> byte[]) =

    //let references = ["FSharp.Core";"mscorlib";"System";"System.Core";"System.Data";"System.IO";"System.Xml";"System.Numerics"]

    let IsSignatureDataResource         (r: ILResource) = String.hasPrefix r.Name FSharpSignatureDataResourceName
    let GetSignatureDataResourceName    (r: ILResource) = String.dropPrefix (String.dropPrefix r.Name FSharpSignatureDataResourceName) "."

    // load signature data
    let GetSignatureData ((filename:string), ilScopeRef, (ilModule:ILModuleDef option), (bytes:byte[])) : TastPickle.PickledDataWithReferences<PickledCcuInfo> = 
        TastPickle.unpickleObjWithDanglingCcus filename ilScopeRef ilModule TastPickle.unpickleCcuInfo bytes

    let tcConfig = TcConfig()
    let tcImports = TcImports()
    let ilGlobals = IL.EcmaMscorlibILGlobals

    let sigDataReaders ilModule =
        let resources = ilModule.Resources.AsList
        let sigDataReaders = 
            [ for iresource in resources do
                if IsSignatureDataResource iresource then 
                    let ccuName = GetSignatureDataResourceName iresource 
                    let bytes = iresource.Bytes
                    yield (ccuName, bytes) ]
        sigDataReaders

    let LoadMod ccuName =
        let fileName = ccuName + ".dll"
        let bytes = readAllBytes fileName
        let opts = ILBinaryReader.mkDefault ilGlobals
        let reader = ILBinaryReader.OpenILModuleReaderFromBytes fileName bytes opts
        reader.ILModuleDef //reader.ILAssemblyRefs

    let memoize_mod = new MemoizationTable<_,_> (LoadMod, keyComparer=HashIdentity.Structural)

    let LoadSig ccuName =
        let fileName =
            if ccuName = "FSharp.Core" then ccuName + ".sigdata"
            else ccuName + ".dll"
        let ilScopeRef = ILScopeRef.Assembly (mkSimpleAssRef ccuName)
        let ilModule = memoize_mod.Apply ccuName
        let bytes =
            if ccuName = "FSharp.Core" then
                readAllBytes fileName
            else
                sigDataReaders ilModule |> List.map snd |> List.head
        let data = GetSignatureData (fileName, ilScopeRef, Some ilModule, bytes)
        data

    let memoize_sig = new MemoizationTable<_,_> (LoadSig, keyComparer=HashIdentity.Structural)

    let GetCustomAttributesOfIlModule (ilModule:ILModuleDef) = 
        (match ilModule.Manifest with Some m -> m.CustomAttrs | None -> ilModule.CustomAttrs).AsList 

    let GetAutoOpenAttributes ilg ilModule = 
        ilModule |> GetCustomAttributesOfIlModule |> List.choose (TryFindAutoOpenAttr ilg)

    let GetInternalsVisibleToAttributes ilg ilModule = 
        ilModule |> GetCustomAttributesOfIlModule |> List.choose (TryFindInternalsVisibleToAttr ilg)

    let HasAnyFSharpSignatureDataAttribute ilModule = 
        let attrs = GetCustomAttributesOfIlModule ilModule
        List.exists IsSignatureDataVersionAttr attrs

    let mkCcuInfo ilg ilScopeRef ilModule ccu =
          { ILScopeRef = ilScopeRef
            FSharpViewOfMetadata = ccu
            AssemblyAutoOpenAttributes = GetAutoOpenAttributes ilg ilModule
            AssemblyInternalsVisibleToAttributes = GetInternalsVisibleToAttributes ilg ilModule
#if EXTENSIONTYPING
            IsProviderGenerated = false
            TypeProviders = []
#endif
            FSharpOptimizationData = notlazy None }

    let GetCcuIL m ccuName =
        let auxModuleLoader = function
            | ILScopeRef.Local -> failwith "Unsupported reference"
            | ILScopeRef.Module x -> memoize_mod.Apply x.Name
            | ILScopeRef.Assembly x -> memoize_mod.Apply x.Name
        let ilModule = memoize_mod.Apply ccuName
        let fileName = ilModule.Name
        let ilScopeRef = ILScopeRef.Assembly (mkSimpleAssRef ccuName)
        let invalidateCcu = new Event<_>()
        let ccu = Import.ImportILAssembly(tcImports.GetImportMap,m,auxModuleLoader,ilScopeRef,tcConfig.implicitIncludeDir,Some fileName,ilModule,invalidateCcu.Publish)
        let ccuInfo = mkCcuInfo ilGlobals ilScopeRef ilModule ccu
        ccuInfo, None

    let GetCcuFS m ccuName =
        let data = memoize_sig.Apply ccuName
        let ilModule = memoize_mod.Apply ccuName
        let fileName = ilModule.Name
        let ilScopeRef = ILScopeRef.Assembly (mkSimpleAssRef ccuName)
        let GetRawTypeForwarders ilModule =
            match ilModule.Manifest with 
            | Some manifest -> manifest.ExportedTypes
            | None -> mkILExportedTypes []
#if EXTENSIONTYPING
        let invalidateCcu = new Event<_>()
#endif
        let minfo : PickledCcuInfo = data.RawData
        let codeDir = minfo.compileTimeWorkingDir
        let ccuData : CcuData = 
              { ILScopeRef = ilScopeRef
                Stamp = newStamp()
                FileName = Some fileName 
                QualifiedName = Some (ilScopeRef.QualifiedName)
                SourceCodeDirectory = codeDir
                IsFSharp = true
                Contents = minfo.mspec
#if EXTENSIONTYPING
                InvalidateEvent=invalidateCcu.Publish
                IsProviderGenerated = false
                ImportProvidedType = (fun ty -> Import.ImportProvidedType (tcImports.GetImportMap()) m ty)
#endif
                UsesFSharp20PlusQuotations = minfo.usesQuotations
                MemberSignatureEquality = (fun ty1 ty2 -> Tastops.typeEquivAux EraseAll (tcImports.GetTcGlobals()) ty1 ty2)
                TypeForwarders = Import.ImportILAssemblyTypeForwarders(tcImports.GetImportMap, m, GetRawTypeForwarders ilModule)
                }
        let ccu = CcuThunk.Create(ccuName, ccuData)
        let ccuInfo = mkCcuInfo ilGlobals ilScopeRef ilModule ccu
        ccuInfo, Some data

    let rec GetCcu m ccuName =
        let ilModule = memoize_mod.Apply ccuName
        if HasAnyFSharpSignatureDataAttribute ilModule then
            GetCcuFS m ccuName
        else
            GetCcuIL m ccuName

    let fixupCcuInfo refCcusUnfixed =
        let refCcus = refCcusUnfixed |> List.map fst
        let findCcuInfo name = (refCcus |> List.find (fun x -> x.FSharpViewOfMetadata.AssemblyName = name)).FSharpViewOfMetadata
        let fixup (data: TastPickle.PickledDataWithReferences<_>) =
            data.Fixup findCcuInfo |> ignore //TODO: perhaps use OptionalFixup
        refCcusUnfixed |> List.choose snd |> List.iter fixup
        refCcus

    let m = range.Zero
    let refCcusUnfixed = references |> List.map (GetCcu m)
    let refCcus = fixupCcuInfo refCcusUnfixed
    let sysCcus = refCcus |> List.filter (fun x -> x.FSharpViewOfMetadata.AssemblyName <> "FSharp.Core")
    let fslibCcu = refCcus |> List.find (fun x -> x.FSharpViewOfMetadata.AssemblyName = "FSharp.Core")

    let ccuInfos = [fslibCcu] @ sysCcus
    let ccuMap = ccuInfos |> List.map (fun ccuInfo -> ccuInfo.FSharpViewOfMetadata.AssemblyName, ccuInfo) |> Map.ofList

    // search over all imported CCUs for each cached type
    let ccuHasType (ccu : CcuThunk) (nsname : string list) (tname : string) =
        match (Some ccu.Contents, nsname) ||> List.fold (fun entityOpt n -> match entityOpt with None -> None | Some entity -> entity.ModuleOrNamespaceType.AllEntitiesByCompiledAndLogicalMangledNames.TryFind n) with
        | Some ns ->
                match Map.tryFind tname ns.ModuleOrNamespaceType.TypesByMangledName with
                | Some _ -> true
                | None -> false
        | None -> false

    // Search for a type
    let tryFindSysTypeCcu nsname typeName =
        let search = sysCcus |> List.tryFind (fun ccuInfo -> ccuHasType ccuInfo.FSharpViewOfMetadata nsname typeName)
        match search with
        | Some x -> Some x.FSharpViewOfMetadata
        | None ->
            printfn "Cannot find type %s.%s" (String.concat "." nsname) typeName
            None

    let tcGlobals = TcGlobals(tcConfig.compilingFslib, ilGlobals, fslibCcu.FSharpViewOfMetadata,
                                tcConfig.implicitIncludeDir, tcConfig.mlCompatibility,
                                tcConfig.isInteractive, tryFindSysTypeCcu, tcConfig.emitDebugInfoInQuotations,
                                (tcConfig.primaryAssembly.Name = "mscorlib"), tcConfig.noDebugData )

#if DEBUG
    // the global_g reference cell is used only for debug printing
    do global_g := Some tcGlobals
#endif
    // do this prior to parsing, since parsing IL assembly code may refer to mscorlib
    do tcImports.SetCcuMap(ccuMap)
    do tcImports.SetTcGlobals(tcGlobals)

    let niceNameGen = NiceNameGenerator()
    let amap = tcImports.GetImportMap()
    let rng = rangeN Lexhelp.stdinMockFilename 0

    let assemblyName = "Project"
    let ccus = ccuInfos |> List.map (fun x -> x.FSharpViewOfMetadata, x.AssemblyAutoOpenAttributes, x.AssemblyInternalsVisibleToAttributes)
    let tcEnv = CreateInitialTcEnv (tcGlobals, amap, rng, assemblyName, ccus)
    let tcState = GetInitialTcState (rangeStartup, assemblyName, tcConfig, tcGlobals, tcImports, niceNameGen, tcEnv)
    let ctok = CompilationThreadToken()
    let reactorOps = 
        let ct = CancellationToken()
        { new IReactorOperations with 
                member __.EnqueueAndAwaitOpAsync (desc, op) = async.Return (op ctok ct)
                member __.EnqueueOp (desc, op) = (op ctok) }

    member x.ParseScript (mainInputFileName, source) =
        // Note: projectSourceFiles is only used to compute isLastCompiland, and is ignored if Build.IsScript(mainInputFileName) is true (which it is in this case).
        let projectSourceFiles = []
        let parseErrors, _matchPairs, inputOpt, anyErrors =
            Parser.ParseOneFile (ctok, source, false, true, mainInputFileName, projectSourceFiles, tcConfig)
        let dependencyFiles = [] // interactions have no dependencies
        let parseResults = FSharpParseFileResults (parseErrors, inputOpt, anyErrors, dependencyFiles)
        parseResults

    member x.ParseAndCheckScript (mainInputFileName, source) =
         // Note: projectSourceFiles is only used to compute isLastCompiland, and is ignored if Build.IsScript(mainInputFileName) is true (which it is in this case).
         let projectSourceFiles = []
         let parseErrors, _matchPairs, inputOpt, anyErrors =
             Parser.ParseOneFile (ctok, source, false, true, mainInputFileName, projectSourceFiles, tcConfig)
         let dependencyFiles = [] // interactions have no dependencies
         let parseResults = FSharpParseFileResults (parseErrors, inputOpt, anyErrors, dependencyFiles)
         let loadClosure = None
         let backgroundErrors = []
         let tcResults =
            Parser.TypeCheckOneFile (parseResults,
                                     source,
                                     mainInputFileName,
                                     "project",
                                     tcConfig,
                                     tcGlobals,
                                     tcImports,
                                     tcState,
                                     loadClosure,
                                     backgroundErrors,
                                     reactorOps,
                                     (fun () -> true),
                                     None)

         match tcResults with 
         | tcErrors, Parser.TypeCheckAborted.No scope, tcImplFiles ->
             let errors = [|  yield! parseErrors; yield! tcErrors |]
             let typeCheckResults = FSharpCheckFileResults (errors, Some scope, dependencyFiles, None, reactorOps)   
             let projectResults = FSharpCheckProjectResults (true, errors, Some(tcGlobals, tcImports, scope.ThisCcu, scope.CcuSig, [scope.ScopeSymbolUses], None, None, mkSimpleAssRef "stdin", tcState.TcEnvFromImpls.AccessRights, Some tcImplFiles, dependencyFiles), reactorOps)
             parseResults, typeCheckResults, projectResults
         | _ -> 
             failwith "unexpected aborted"


//-------------------------------------------------------------------------
// AstPrint
//-------------------------------------------------------------------------

module AstPrint =

    let attribsOfSymbol (s:FSharpSymbol) =
        let tryOr f def =
            try f() with _ -> def
        [ match s with
            | :? FSharpField as v ->
                yield "field"
                if v.IsCompilerGenerated then yield "compgen"
                if v.IsDefaultValue then yield "default"
                if v.IsMutable then yield "mutable"
                if v.IsVolatile then yield "volatile"
                if v.IsStatic then yield "static"
                if v.IsLiteral then yield sprintf "%A" v.LiteralValue.Value

            | :? FSharpEntity as v ->
                v.TryFullName |> ignore // check there is no failure here
                match v.BaseType with
                | Some t when t.HasTypeDefinition && t.TypeDefinition.TryFullName.IsSome ->
                    yield sprintf "inherits %s" t.TypeDefinition.FullName
                | _ -> ()
                if v.IsNamespace then yield "namespace"
                if v.IsFSharpModule then yield "module"
                if v.IsByRef then yield "byref"
                if v.IsClass then yield "class"
                if v.IsDelegate then yield "delegate"
                if v.IsEnum then yield "enum"
                if v.IsFSharpAbbreviation then yield "abbrev"
                if v.IsFSharpExceptionDeclaration then yield "exception"
                if v.IsFSharpRecord then yield "record"
                if v.IsFSharpUnion then yield "union"
                if v.IsInterface then yield "interface"
                if v.IsMeasure then yield "measure"
#if EXTENSIONTYPING
                if v.IsProvided then yield "provided"
                if v.IsStaticInstantiation then yield "static_inst"
                if v.IsProvidedAndErased then yield "erased"
                if v.IsProvidedAndGenerated then yield "generated"
#endif
                if v.IsUnresolved then yield "unresolved"
                if v.IsValueType then yield "valuetype"

            | :? FSharpMemberOrFunctionOrValue as v ->
                yield "owner: " + (tryOr (fun () -> v.EnclosingEntity.CompiledName) "<unknown>")
                if v.IsActivePattern then yield "active_pattern"
                if v.IsDispatchSlot then yield "dispatch_slot"
                if v.IsModuleValueOrMember && not v.IsMember then yield "val"
                if v.IsMember then yield "member"
                if v.IsProperty then yield "property"
                if v.IsExtensionMember then yield "extension_member"
                if v.IsPropertyGetterMethod then yield "property_getter"
                if v.IsPropertySetterMethod then yield "property_setter"
                if v.IsEvent then yield "event"
                if v.EventForFSharpProperty.IsSome then yield "property_event"
                if v.IsEventAddMethod then yield "event_add"
                if v.IsEventRemoveMethod then yield "event_remove"
                if v.IsTypeFunction then yield "type_func"
                if v.IsCompilerGenerated then yield "compiler_gen"
                if v.IsImplicitConstructor then yield "implicit_ctor"
                if v.IsMutable then yield "mutable"
                if v.IsOverrideOrExplicitInterfaceImplementation then yield "override_impl"
                if not v.IsInstanceMember then yield "static"
                if v.IsInstanceMember && not v.IsInstanceMemberInCompiledCode && not v.IsExtensionMember then yield "funky"
                if v.IsExplicitInterfaceImplementation then yield "interface_impl"
                yield sprintf "%A" v.InlineAnnotation
                // if v.IsConstructorThisValue then yield "ctorthis"
                // if v.IsMemberThisValue then yield "this"
                // if v.LiteralValue.IsSome then yield "literal"
            | _ -> () ]

    let rec printFSharpDecls prefix decls = seq {
        let mutable i = 0
        for decl in decls do
            i <- i + 1
            match decl with
            | FSharpImplementationFileDeclaration.Entity (e, sub) ->
                yield sprintf "%s%i) ENTITY: %s %A" prefix i e.CompiledName (attribsOfSymbol e)
                if not (Seq.isEmpty e.Attributes) then
                    yield sprintf "%sattributes: %A" prefix (Seq.toList e.Attributes)
                if not (Seq.isEmpty e.DeclaredInterfaces) then
                    yield sprintf "%sinterfaces: %A" prefix (Seq.toList e.DeclaredInterfaces)
                yield ""
                yield! printFSharpDecls (prefix + "\t") sub
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth, args, body) ->
                yield sprintf "%s%i) METHOD: %s %A" prefix i meth.CompiledName (attribsOfSymbol meth)
                yield sprintf "%stype: %A" prefix meth.FullType
                yield sprintf "%sargs: %A" prefix args
                // if not meth.IsCompilerGenerated then
                yield sprintf "%sbody: %A" prefix body
                yield ""
            | FSharpImplementationFileDeclaration.InitAction (expr) ->
                yield sprintf "%s%i) ACTION" prefix i
                yield sprintf "%s%A" prefix expr
                yield ""
    }
