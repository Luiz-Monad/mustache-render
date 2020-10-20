// Learn more about F# at http://fsharp.org

open System
open FSharp
open FParsec

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

[<EntryPoint>]
let main argv =

    let arg0 = argv |> Seq.head
    let arg1 = argv |> Seq.skip 1 |> Seq.head
    let file = System.IO.File.ReadAllText arg0
    
    match run templ file with
    | Failure ( _, f, _ ) -> 
        sprintf "%O" f |> System.Console.Error.WriteLine
        1

    | Success ( s, _, _ ) -> 
        let out = html style (List.map renderHtml s |> String.concat "") 
        System.IO.File.WriteAllText(arg1, out)
        0
