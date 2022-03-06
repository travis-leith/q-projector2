module ScopeParser
open FParsec
open System
open ScopeAst

[<AutoOpen>]
module private Utils =
  let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
  let escapedChar = pchar '\\' >>. (anyOf ['\\'; '\n'; '\r'; '\t'; '\"'])
  let str_ws s = pstring s >>. spaces
  let char_ws c = pchar c >>. spaces

module private Atom =
  let date = 
    pint32 .>> pchar '.' >>= fun y ->
    pint32 .>> pchar '.' >>= fun m ->
    pint32 >>= fun d ->
      try preturn (DateOnly(y,m,d) |> ignore)
      with _ -> fail $"Date format error '{y}.{m}.{d}'"

  let string_ = (manyChars (normalChar <|> escapedChar)) |> between (pstring "\"") (pstring "\"") |>> ignore

  let number = 
    let format = 
      NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowPlusSign ||| NumberLiteralOptions.AllowFraction
    numberLiteral format "Not a number" |>> (fun nl -> nl.String |> ignore)

  let symbol = pchar '`' >>. manyCharsTill normalChar (pchar ' ' <|> pchar ';' <|> newline) |>> ignore

  let parser = 
    choice [
      attempt date
      string_
      number
      symbol
    ] .>> spaces

module private Keyword =
  let parser = 
    manyChars2 letter (letter <|> digit) >>= (fun s ->
      if StringConstants.keywords.Contains s then preturn()
      else fail "not a keyword"
    ) .>> spaces

module private Identifier =
  let parser =
    let isIdentifierFirstChar c = isLetter c || c = '.'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_' || c = '.'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> spaces

module private Function =
  let pParams = between (char_ws '[') (char_ws ']') (sepBy Identifier.parser (char_ws ';')) |>> Set.ofList
  //let parser pUsage = 
  //  let inBraces x = between (char_ws '{') (char_ws '}') x
  //  pipe2 pParams pUsage (fun pars usage -> {Paramaters = pars; IdentifierUsage = usage}) |> inBraces .>> spaces
  let parser pUsage = parse{
    do! skipChar '{'
    let! pars = pParams .>> spaces
    let! body = charsTillString "}" true 10000
    match run pUsage body with
    |Success (usage,_,_) -> return {Paramaters = pars; IdentifierUsage = usage}
    |Failure (e,_,_) -> return! fail e
  }

module private Operator =
  let parser = skipAnyOf StringConstants.specialCharacters >>. spaces

module private Code =
  let parser = skipMany1 (Keyword.parser <|> Atom.parser <|> Operator.parser) |> attempt

module private Comment =
  let parser = (pstring "/ " <|> pstring "//") .>> skipManyTill anyChar newline >>. spaces

module private Usage =
  let pUsage, pUsageRef = createParserForwardedToRef()

  let identifierDetail =
    choice [
      attempt (Function.parser pUsage |>> Some)
      attempt (Code.parser >>. preturn None)
    ] .>> spaces

  let definition = Identifier.parser .>>? skipChar ':' .>> spaces .>>. identifierDetail |>> (Definition >> Some)
  let invokation = many Code.parser >>. Identifier.parser .>> spaces |>> (Invokation >> Some)
  let code = Code.parser >>. preturn None
  let comment = Comment.parser >>. preturn None

  pUsageRef.Value <-
    choice [
      attempt definition
      attempt code
      attempt invokation
      attempt comment
    ] |> many .>> eof |>> List.choose id

  let parser = pUsageRef.Value

module private GlobalScope =
  let parser = Usage.parser |>> (fun x -> {Paramaters = Set.empty; IdentifierUsage = x})

let parse parser s =
  match run parser s with
  | Success(result, _, _)   -> Result.Ok result
  | Failure(errorMsg, _, _) -> Result.Error errorMsg

let parseFile = parse GlobalScope.parser

let parseCode = parse Usage.code
let parseDefinition = parse Usage.definition
let parseInvokaition = parse Usage.invokation
let parseComment = parse Usage.comment
//let parseFunction = parse (Function.parser (many1 Usage.invokation |>> List.choose id))