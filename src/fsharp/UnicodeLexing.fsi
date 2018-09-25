// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.UnicodeLexing

open Microsoft.FSharp.Text
open Internal.Utilities.Text.Lexing

type Lexbuf =  LexBuffer<LexBufferChar>
val internal StringAsLexbuf : string -> Lexbuf
val public FunctionAsLexbuf : (LexBufferChar[] * int * int -> int) -> Lexbuf
val public UnicodeFileAsLexbuf :string * int option * (*retryLocked*) bool -> Lexbuf
