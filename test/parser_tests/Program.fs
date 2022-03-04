open Expecto
open Expecto.Flip
open qprojector
open System

let testAtoms = 
  let testAtom name s f = test name {
    Parser.parseExpression s |> Expect.equal "" (s.TrimEnd() |> f |> Ok)
  }

  let testWithExpectation name s exp = testAtom name s (fun _ -> exp)

  testList "Atoms" [
    testAtom "Integer" "213" (Atom.Number >> Expression.Atom)
    testAtom "Float" "213.4" (Atom.Number >> Expression.Atom)
    testWithExpectation "Date" "2021.01.01" (DateOnly(2021,1,1) |> Atom.Date |> Expression.Atom)
    testWithExpectation "Symbol" "`symbolic " (Atom.Symbol "symbolic" |> Expression.Atom)
    testWithExpectation "String" "\"abcd\"" (Atom.String "abcd" |> Expression.Atom)
    testWithExpectation "EscapedString" "\"ab\n\tcd\"" (Atom.String "ab\n\tcd" |> Expression.Atom)
  ]

let testIdentifier =
  let mixed = test "Mixed" {
    "someId1" |> Parser.parseExpression |> Expect.equal "" ("someId1" |> Identifier.Identifier |> Expression.IdentExpr |> Ok)
  }

  let single = test "Single" {
    "a" |> Parser.parseExpression |> Expect.equal "" ("a" |> Identifier.Identifier |> Expression.IdentExpr |> Ok)
  }

  testList "Identifier" [
    mixed
    single
  ]

let testBinaryExpressions =
  let singleNoSpace = test "SingleNoSpace" {
    let expectation = 
      {
        LeftSide = "1" |> Number |> Atom
        Operator = "*" |> BinaryOperator
        RightSide = "b" |> Identifier.Identifier |> Expression.IdentExpr
      } |> BinaryExpression
    "1*b" |> Parser.parseExpression |> Expect.equal "" (expectation |> Ok)
  }

  let singleWithSpace = test "SingleWithSpacec" {
    let expectation = 
      {
        LeftSide = "b" |> Identifier.Identifier |> Expression.IdentExpr
        Operator = "*" |> BinaryOperator
        RightSide = "1" |> Number |> Atom
      } |> BinaryExpression
    "b * 1" |> Parser.parseExpression |> Expect.equal "" (expectation |> Ok)
  }

  let double = test "Double" {
    let expectation =
      {
        LeftSide = "1" |> Number |> Atom
        Operator = "*" |> BinaryOperator
        RightSide = 
          {
            LeftSide = "b" |> Identifier.Identifier |> Expression.IdentExpr
            Operator = "+" |> BinaryOperator
            RightSide = "3" |> Number |> Atom
          } |> BinaryExpression
      } |> BinaryExpression
    "1 * b +3" |> Parser.parseExpression |> Expect.equal "" (expectation |> Ok)
  }

  let parens = test "parens" {
    let expectation =
      {
        LeftSide =
          {
            LeftSide = "1" |> Number |> Atom
            Operator = "*" |> BinaryOperator
            RightSide = "b" |> Identifier.Identifier |> Expression.IdentExpr
          } |> BinaryExpression
        Operator = "+" |> BinaryOperator
        RightSide = "3" |> Number |> Atom
      } |> BinaryExpression
    "( 1 *b) +3" |> Parser.parseExpression |> Expect.equal "" (expectation |> Ok)
  }

  testList "BinaryExpressions" [
    singleNoSpace
    singleWithSpace
    double
    parens
  ]

let testFunctions = 
  let oneParam = test "OneParam" {
    let expected = 
      {
        Parameters = ["x" |> Identifier] |> Set.ofList
        Body = {
          LeftSide = "1" |> Number |> Atom
          Operator = "+" |> BinaryOperator
          RightSide = "x" |> Identifier |> IdentExpr
        } |> BinaryExpression
      } |> Function |> Ok
    Parser.parseExpression "{[x] 1 + x}" |> Expect.equal "" expected
  }

  let noParam = test "NoParam" {
    let expected = 
      {
        Parameters = Set.empty
        Body = {
          LeftSide = "1" |> Number |> Atom
          Operator = "+" |> BinaryOperator
          RightSide = "x" |> Identifier |> IdentExpr
        } |> BinaryExpression
      } |> Function |> Ok
    Parser.parseExpression "{[] 1 + x}" |> Expect.equal "" expected
  }

  let twoParam = test "TwoParam" {
    let expected = 
      {
        Parameters = ["x" |> Identifier; "y" |> Identifier] |> Set.ofList
        Body = {
          LeftSide = "y" |> Identifier |> IdentExpr
          Operator = "+" |> BinaryOperator
          RightSide = "x" |> Identifier |> IdentExpr
        } |> BinaryExpression
      } |> Function |> Ok
    Parser.parseExpression "{[x; y] y + x}" |> Expect.equal "" expected
  }

  testList "Functions" [
    oneParam
    noParam
    twoParam
  ]

let allTests = testList "Parser" [
  testAtoms
  testIdentifier
  testBinaryExpressions
  testFunctions
]

runTestsWithCLIArgs [] Array.empty allTests |> ignore
//"1 * b +3" |> Parser.parseExpression |> printfn "%A"