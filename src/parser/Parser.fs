namespace qprojector

open FParsec

module private ParseAtom =
  let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
  let escapedChar = pchar '\\' >>. (anyOf ['\\'; '\n'; '\r'; '\t'; '\"'])

  let atom = choice [
    regex "\d{4}\.\d{2}\.\d{2}" |>> Atom.Date
    (manyChars (normalChar <|> escapedChar)) |> between (pstring "\"") (pstring "\"") |>> Atom.String
    regex "\d+\.?\d*" |>> Atom.Number
    pchar '`' >>. manyCharsTill normalChar (pchar ' ' <|> pchar ';' <|> newline) |>> Atom.Symbol
  ]

module Parser =
  let parseString s =
    match run ParseAtom.atom s with
    | Success(result, _, _)   -> Result.Ok result
    | Failure(errorMsg, _, _) -> Result.Error errorMsg
  