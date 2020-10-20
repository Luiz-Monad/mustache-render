<Query Kind="FSharpProgram">
  <Reference Relative="..\.nuget\packages\fparsec\1.1.1\lib\net45\FParsec.dll">&lt;NuGet&gt;\fparsec\1.1.1\lib\net45\FParsec.dll</Reference>
  <Reference Relative="..\.nuget\packages\fparsec\1.1.1\lib\net45\FParsecCS.dll">&lt;NuGet&gt;\fparsec\1.1.1\lib\net45\FParsecCS.dll</Reference>
  <Namespace>FParsec</Namespace>
</Query>


open System
open FSharp
open FParsec

let B (p: Parser<_,_>) stream =
    let u = stream :> obj :?> CharStream<unit>
    let pos = u.Position |> string
    p.Dump(pos) |> ignore
    p stream
    
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        let result = (sprintf "%O" reply.Result).Replace("\n", "\\n")
        printfn "%A: Leaving %s (%A) %s" stream.Position label reply.Status result
        reply

//let charsTillString a b c = charsTillString a b c |> BP
//let between a b c = between a b c |> BP

//**************************************

type VarSpec =
    | Unescaped
    | UnescapedAmper
    | Escaped

type Tmpl = 
    | Text        of string
    | Variable    of string * VarSpec
    | Include     of string
    | Section     of string * Tmpl list
    | InvSection  of string * Tmpl list
    | Comment     of string

let variable spec name = Variable (name, spec)

let str = pstring
let anyNot s tag = charsTillString s false Int32.MaxValue |>> tag
let anyNotIn s cs tag = many1CharsTill (noneOf cs) (str s) |>> tag

let trueTag = between (str "{{#") (str "}}") (anyNot "}}" id) //<!> "true"
let falseTag = between (str "{{^") (str "}}") (anyNot "}}" id) //<!> "false"
let endTag = between (str "{{/") (str "}}") (anyNot "}}" id) //<!> "end"
let comm = between (str "{{!") (str "}}") (anyNot "}}" Comment) //<!> "comm"
let inc = between (str "{{>") (str "}}") (anyNot "}}" Include) //<!> "inc"
let evar = between (str "{{{") (str "}}}") (anyNot "}}}" (variable Unescaped)) //<!> "evar"
let evar2 = between (str "{{&") (str "}}") (anyNot "}}" (variable UnescapedAmper)) //<!> "evar2"
let tvar = (str "{{") >>. (anyNotIn "}}" "&#^/!{}>" (variable Escaped)) //<!> "tvar"
let text = many1Chars (noneOf "{") |>> Text //<!> "txt"
let single = str "{" |>> Text //<!> "single"

let value, valueRef = createParserForwardedToRef<Tmpl, unit>()

let section = trueTag .>>. (many (attempt value)) .>> endTag |>> Section //<!> "section"
let invSection = falseTag .>>. (many (attempt value)) .>> endTag |>> InvSection //<!> "invsection"

do valueRef := choice [
    comm
    inc
    evar
    evar2
    section
    invSection
    tvar
    text
    single
    ]

let templ = many ( notFollowedBy eof >>. value ) .>> eof

//**************************************

let file = System.IO.File.ReadAllText(@"C:\extern\workbench\parental-control\languagefilter\api\templ-database\data_class.mustache")
//file |> Dump |> ignore
//run templ file |> Dump |> ignore

let rec dumpAst = function
    | Text s -> box {| text = s |}
    | Variable (s, t) -> 
        match t with
        | Unescaped -> box {| var = s; unescaped = true |}
        | UnescapedAmper -> box {| var = s; unescaped = true; amper = true |}
        | Escaped -> box {| var = s; escaped = true |}
    | Include s -> box {| ``include`` = s |}
    | Section (s, c) -> box {| section = s; children = List.map dumpAst c |}
    | InvSection (s, c) -> box {| ``not`` = true; section = s; children = List.map dumpAst c |}
    | Comment s -> box {| comment = s |}

//match run templ file with
//| Success ( s, _, _ ) -> s |> List.map dumpAst |> Dump |> ignore
//| Failure ( _, f, _ ) -> sprintf "%O" f |> Dump |> ignore

let rec renderAst = function
    | Text s -> s
    | Variable (s, t) -> 
        match t with
        | Unescaped -> sprintf "{{{%s}}}" s
        | UnescapedAmper -> sprintf "{{&%s}}" s
        | Escaped -> sprintf "{{%s}}" s
    | Include s -> sprintf "{{>%s}}" s
    | Section (s, c) -> sprintf "{{#%s}}%s{{/%s}}" s (List.map renderAst c |> String.concat "" ) s
    | InvSection (s, c) -> sprintf "{{^%s}}%s{{/%s}}" s (List.map renderAst c |> String.concat "" ) s
    | Comment s -> sprintf "{{!%s}}" s

//match run templ file with
//| Success ( s, _, _ ) -> s |> List.map renderAst |> String.concat "" |> Dump |> ignore
//| Failure ( _, f, _ ) -> sprintf "%O" f |> Dump |> ignore

let rec renderJson = function
    | Text s -> sprintf "{\"txt\":\"%s\"}" (s.Replace("\n" ,"\\n").Replace("\"", "\\\""))
    | Variable (s, t) -> 
        match t with
        | Unescaped -> sprintf "{\"uvar\":\"%s\"}" s
        | UnescapedAmper -> sprintf "{\"auvar\":\"%s\"}" s
        | Escaped -> sprintf "{\"var\":\"%s\"}" s
    | Include s -> sprintf "{\"include\":\"%s\"}" s
    | Section (s, c) -> sprintf "\n{\"%s\":[%s]}" s (List.map renderJson c |> String.concat "," )
    | InvSection (s, c) -> sprintf "\n{\"not\":{\"%s\":[%s]}}" s (List.map renderJson c |> String.concat "," )
    | Comment s -> sprintf "{\"comment\":\"%s\"}" s

//match run templ file with
//| Success ( s, _, _ ) -> sprintf "[%s]" (List.map renderJson s |> String.concat ",") |> Dump |> ignore
//| Failure ( _, f, _ ) -> sprintf "%O" f |> Dump |> ignore

let rec renderHtml = function
    | Text s -> sprintf "<span class=\"text\">%s</span>" ((System.Security.SecurityElement.Escape s).Replace(" ", "\u00A0").Replace("\n", "\\n</span><br/><span class=\"text\">"))
    | Variable (s, t) -> 
        match t with
        | Unescaped -> sprintf "<span class=\"uvar\">%s</span>" s
        | UnescapedAmper -> sprintf "<span class=\"auvar\">%s</span>" s
        | Escaped -> sprintf "<span class=\"var\">%s</span>" s
    | Include s -> sprintf "<span class=\"include\">%s</span>" s
    | Section (s, c) -> sprintf "<div class=\"section\"><span class=\"name\">%s</span>%s</div>" s (List.map renderHtml c |> String.concat "" )
    | InvSection (s, c) -> sprintf "<div class=\"invsection\"><span class=\"invname\">%s</span>%s</div>" s (List.map renderHtml c |> String.concat "" )
    | Comment s -> sprintf "<span class=\"comment\">%s</span>" s

let ToDump o = XElement ("LINQPad.HTML" |> XName.Get, XDocument.Parse(string o).Root )

let html = sprintf "<html><head><style>%s</style></head><body>%s</body></html>" 
let style = "\
    .var { \
        color: #ebdc34; \
    } \
    .uvar { \
        color: #34ebd6; \
    } \
    .uavar { \
        color: #eb5334; \
    } \
    .text { \
        border: 1px dashed #464646; \
    } \
    .section { \
        margin: 1px 1px; \
        padding: 2px 2px; \
        border: 2px solid #547530; \
        display: inline-block; \
    } \
    .section .name { \
        font-size: 70%; \
        color: #7bbd35; \
        display: block; \
        float: right; \
    } \
    .invsection { \
        margin: 1px 1px; \
        padding: 2px 2px; \
        border: 2px solid #b56447; \
        display: inline-block; \
    } \
    .invsection .invname { \
        font-size: 70%; \
        color: #b56447; \
        display: block; \
        float: right; \
    } \
    "

match run templ file with
| Success ( s, _, _ ) -> html style (List.map renderHtml s |> String.concat "") |> ToDump |> Dump |> ignore
| Failure ( _, f, _ ) -> sprintf "%O" f |> Dump |> ignore
