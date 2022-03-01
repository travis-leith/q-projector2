namespace qprojector

open FParsec

module Parser =
  module private Atom =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pchar '\\' >>. (anyOf ['\\'; '\n'; '\r'; '\t'; '\"'])

    let date = regex "\d{4}\.\d{2}\.\d{2}" |>> Atom.Date
    let string_ = (manyChars (normalChar <|> escapedChar)) |> between (pstring "\"") (pstring "\"") |>> Atom.String
    let number = regex "\d+\.?\d*" |>> Atom.Number
    let symbol = pchar '`' >>. manyCharsTill normalChar (pchar ' ' <|> pchar ';' <|> newline) |>> Atom.Symbol
    let parser = 
      choice [
        date
        string_
        number
        symbol
      ]

  module private Identifier =
    let parser = regex "[a-zA-Z]{1}[\d\w]*" |>> Identifier.Identifier

  module private BinaryOperator =
    let parser = ["+"; "-"; "*"; "%"; "="; "<>"; "~"; "<"; "<="; ">="; ">"; "|"; "&"; "#"; "^"] |> Seq.map pstring |> choice |>> BinaryOperator

  module private BinaryExpression =
    let parser expressionParser = pipe3 expressionParser BinaryOperator.parser expressionParser (fun left op right -> {LeftSide = left; Operator = op; RightSide = right})

  module private Expression =
    let expr, parser = createParserForwardedToRef()
    let atom = Atom.parser |>> Expression.Atom
    let assignment = Identifier.parser .>> skipMany (pchar ':') .>>. expr |>> Expression.Assignment
    let identifier = Identifier.parser |>> Expression.Identifier
    let binaryExpression = BinaryExpression.parser expr |>> Expression.BinaryExpression

    let choiceParser = 
      [
        atom
        assignment
        identifier
        binaryExpression
      ] |> choice
    //ref cells are the only way to support recursive grammars in FParsec
    parser.Value <- choiceParser
    
  let parse parser s =
    match run parser s with
    | Success(result, _, _)   -> Result.Ok result
    | Failure(errorMsg, _, _) -> Result.Error errorMsg

  let parseExpression = parse Expression.parser.Value
    
  