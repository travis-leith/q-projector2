module qprojector.Parser

open FParsec
open System

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
      try preturn (DateOnly(y,m,d) |> Date)
      with _ -> fail $"Date format error '{y}.{m}.{d}'"

  let string_ = (manyChars (normalChar <|> escapedChar)) |> between (pstring "\"") (pstring "\"") |>> Atom.String

  let number = 
    let format = 
      NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowPlusSign ||| NumberLiteralOptions.AllowFraction
    numberLiteral format "Not a number" |>> (fun nl -> nl.String |> Number)

  let symbol = pchar '`' >>. manyCharsTill normalChar (pchar ' ' <|> pchar ';' <|> newline) |>> Atom.Symbol

  let parser = 
    choice [
      attempt date
      string_
      number
      symbol
    ] .>> spaces

module private Identifier =
  let parser =
    let isIdentifierFirstChar c = isLetter c
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> spaces |>> Identifier.Identifier

module private BinaryExpression =
  type Assoc = Associativity
  let parser pLeftSide =
    let opp = new OperatorPrecedenceParser<Expression,unit,unit>()
    opp.TermParser <- pLeftSide

    ["+"; "-"; "*"; "%"; "="; "<>"; "~"; "<"; "<="; ">="; ">"; "|"; "&"; "#"; "^"]
    |> List.iter(fun opString ->
      opp.AddOperator(InfixOperator(opString, spaces, 1, Assoc.Right, fun x y -> {LeftSide = x; Operator = opString |> BinaryOperator; RightSide = y} |> BinaryExpression))
    )
    opp.ExpressionParser

module private Function =
  let pParams = between (char_ws '[') (char_ws ']') (sepBy Identifier.parser (char_ws ';')) |>> Set.ofList
  let parser pExpression = 
    let inBraces x = between (char_ws '{') (char_ws '}') x
    pipe2 pParams pExpression (fun pars expr -> {Parameters = pars; Body = expr}) |>> Function |> inBraces

module private Expression =
  let expr, exprRef = createParserForwardedToRef()
  let atom = Atom.parser |>> Expression.Atom
  let assignment = Identifier.parser .>> skipMany (pchar ':') .>>. expr |>> Expression.Assignment
  let identifier = Identifier.parser |>> Expression.IdentExpr

  let pElement = choice [
    attempt atom
    attempt identifier
    attempt assignment
    between (str_ws "(") (str_ws ")") expr
  ]
  let choiceParser = choice [
    BinaryExpression.parser pElement
    Function.parser expr
    pElement
  ]
  //ref cells are the recommended way to support recursive grammars in FParsec
  exprRef.Value <- choiceParser
  let parser = exprRef.Value
 
let terminateStatement = choice [
  char_ws ';'
  spaces
  eof
]

let statement = Expression.parser .>> terminateStatement

let file = many1 statement

let parse parser s =
  match run parser s with
  | Success(result, _, _)   -> Result.Ok result
  | Failure(errorMsg, _, _) -> Result.Error errorMsg

let parseStatement = parse statement
let parseFile = parse file
    
  