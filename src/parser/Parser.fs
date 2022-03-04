namespace qprojector

open FParsec
open System

module Parser =
  module private Utils =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pchar '\\' >>. (anyOf ['\\'; '\n'; '\r'; '\t'; '\"'])

  module private Atom =
    let date = 
      pint32 .>> pchar '.' >>= fun y ->
      pint32 .>> pchar '.' >>= fun m ->
      pint32 >>= fun d ->
        try preturn (DateOnly(y,m,d) |> Date)
        with _ -> fail $"Date format error '{y}.{m}.{d}'"

    let string_ = (manyChars (Utils.normalChar <|> Utils.escapedChar)) |> between (pstring "\"") (pstring "\"") |>> Atom.String

    let number = 
      let format = 
        NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowPlusSign ||| NumberLiteralOptions.AllowFraction
      numberLiteral format "Not a number" |>> (fun nl -> nl.String |> Number)

    let symbol = pchar '`' >>. manyCharsTill Utils.normalChar (pchar ' ' <|> pchar ';' <|> newline) |>> Atom.Symbol

    let parser = 
      choice [
        attempt date
        string_
        number
        symbol
      ]

  module private Identifier =
    let parser =
      let isIdentifierFirstChar c = isLetter c
      let isIdentifierChar c = isLetter c || isDigit c || c = '_'

      many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
      .>> spaces |>> Identifier.Identifier

  //module private BinaryOperator =
  //  let core = ["+"; "-"; "*"; "%"; "="; "<>"; "~"; "<"; "<="; ">="; ">"; "|"; "&"; "#"; "^"] |> Seq.map pstring |> choice |>> BinaryOperator
  //  let parser = spaces >>. core .>> spaces

  //module private BinaryExpression =
  //  let parser atomParser expressionParser = pipe3 atomParser BinaryOperator.parser expressionParser (fun left op right -> {LeftSide = left; Operator = op; RightSide = right})

  let ws = spaces
  let str_ws s = pstring s >>. ws

  type Assoc = Associativity

  let binaryExpression pLeftSide rightSide =
    let opp = new OperatorPrecedenceParser<Expression,unit,unit>()
    let term = (pLeftSide .>> ws) <|> between (str_ws "(") (str_ws ")") rightSide
    opp.TermParser <- term

    opp.AddOperator(InfixOperator("+", ws, 1, Assoc.Right, fun x y -> {LeftSide = x; Operator = "+" |> BinaryOperator; RightSide = y} |> BinaryExpression))
    opp.AddOperator(InfixOperator("*", ws, 1, Assoc.Right, fun x y -> {LeftSide = x; Operator = "*" |> BinaryOperator; RightSide = y} |> BinaryExpression))
    opp.ExpressionParser

  module private Expression =
    let expr, exprRef = createParserForwardedToRef()
    let atom = Atom.parser |>> Expression.Atom
    let assignment = Identifier.parser .>> skipMany (pchar ':') .>>. expr |>> Expression.Assignment
    let identifier = Identifier.parser |>> Expression.Identifier
    //let binaryExpression = BinaryExpression.parser atom expr |>> Expression.BinaryExpression
    let pLeftSide = choice [
      attempt atom
      attempt identifier
      attempt assignment
    ]
    let choiceParser = 
      [
        binaryExpression pLeftSide expr
        pLeftSide
      ] |> choice
    //ref cells are the recommended way to support recursive grammars in FParsec
    exprRef.Value <- choiceParser
    let parser = exprRef.Value
 
  let statement = Expression.parser .>> eof

  let parse parser s =
    match run parser s with
    | Success(result, _, _)   -> Result.Ok result
    | Failure(errorMsg, _, _) -> Result.Error errorMsg

  let parseExpression = parse statement
    
  