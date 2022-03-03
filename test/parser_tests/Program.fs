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
  ]

let testIdentifier =
  let mixed = test "Mixed" {
    "someId1" |> Parser.parseExpression |> Expect.equal "" ("someId1" |> Identifier.Identifier |> Expression.Identifier |> Ok)
  }

  let single = test "Single" {
    "a" |> Parser.parseExpression |> Expect.equal "" ("a" |> Identifier.Identifier |> Expression.Identifier |> Ok)
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
        RightSide = "b" |> Identifier.Identifier |> Expression.Identifier
      } |> BinaryExpression
    "1*b" |> Parser.parseExpression |> Expect.equal "" (expectation |> Ok)
  }

  let singleWithSpace = test "SingleWithSpacec" {
    let expectation = 
      {
        LeftSide = "1" |> Number |> Atom
        Operator = "*" |> BinaryOperator
        RightSide = "b" |> Identifier.Identifier |> Expression.Identifier
      } |> BinaryExpression
    "1 * b" |> Parser.parseExpression |> Expect.equal "" (expectation |> Ok)
  }

  testList "BinaryExpressions" [
    singleNoSpace
    singleWithSpace
  ]

let allTests = testList "Parser" [
  testAtoms
  testIdentifier
  testBinaryExpressions
]

runTestsWithCLIArgs [] Array.empty allTests |> ignore