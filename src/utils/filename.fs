// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

module internal Internal.Utilities.Filename

#if FABLE_COMPILER
open Internal.Utilities
#endif
open System.IO
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 

exception IllegalFileNameChar of string * char

/// The set of characters which may not be used in a path.
/// This is saved here because Path.GetInvalidPathChars() allocates and returns
/// a new array each time it's called (by necessity, for security reasons).
/// This is only used within `checkPathForIllegalChars`, and is only read from.
let illegalPathChars =
#if FABLE_COMPILER
    let chars = Seq.toArray "<>|\"\b\0\t" //TODO: more
#else
    let chars = Path.GetInvalidPathChars ()
#endif
    chars

type private PathState =
    | Legal
    | Illegal of path: string * illegalChar: char

let private checkPathForIllegalChars =
    let cache = System.Collections.Concurrent.ConcurrentDictionary<string, PathState>()
    fun (path: string) ->
        match cache.TryGetValue path with
        | true, Legal -> () 
        | true, Illegal (path, c) -> raise(IllegalFileNameChar(path, c))
        | _ ->
            let len = path.Length
            for i = 0 to len - 1 do
                let c = path.[i]
        
                // Determine if this character is disallowed within a path by
                // attempting to find it in the array of illegal path characters.
                for badChar in illegalPathChars do
                    if c = badChar then
                        cache.[path] <- Illegal(path, c)
                        raise(IllegalFileNameChar(path, c))
            cache.[path] <- Legal

// Case sensitive (original behaviour preserved).
let checkSuffix (x:string) (y:string) = x.EndsWith(y,System.StringComparison.Ordinal) 

let hasExtension (s:string) = 
    checkPathForIllegalChars s
    let sLen = s.Length
    (sLen >= 1 && s.[sLen - 1] = '.' && s <> ".." && s <> ".") 
#if FABLE_COMPILER
    //TODO: proper implementation
#else
    || Path.HasExtension(s)
#endif

let chopExtension (s:string) =
    checkPathForIllegalChars s
    if s = "." then "" else // for OCaml compatibility
    if not (hasExtension s) then 
        raise (System.ArgumentException("chopExtension")) // message has to be precisely this, for OCaml compatibility, and no argument name can be set
#if FABLE_COMPILER
    s //TODO: proper implementation
#else
    Path.Combine (Path.GetDirectoryName s,Path.GetFileNameWithoutExtension(s))
#endif

let directoryName (s:string) = 
    checkPathForIllegalChars s
    if s = "" then "."
    else 
#if FABLE_COMPILER
        s //TODO: proper implementation
#else
        match Path.GetDirectoryName(s) with 
        | null -> if FileSystem.IsPathRootedShim(s) then s else "."
        | res -> if res = "" then "." else res
#endif

let fileNameOfPath s = 
    checkPathForIllegalChars s
#if FABLE_COMPILER
    s //TODO: proper implementation
#else
    Path.GetFileName(s)
#endif

let fileNameWithoutExtension s = 
    checkPathForIllegalChars s
#if FABLE_COMPILER
    s //TODO: proper implementation
#else
    Path.GetFileNameWithoutExtension(s)
#endif

let trimQuotes (s:string) =
    s.Trim( [|' '; '\"'|] )
