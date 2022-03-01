open Expecto
open Expecto.Flip
open qprojector

let testAtoms = 
  
  let testAtom name s f = test name {
    Parser.parseString s |> Expect.equal "" (s.TrimEnd() |> f |> Ok)
  }

  let testWithExpectation name s exp = testAtom name s (fun _ -> exp)

  testList "Atoms" [
    testAtom "Integer" "213" Atom.Number
    testAtom "Float" "213.4" Atom.Number
    testAtom "Date" "2021.01.01" Atom.Date
    testWithExpectation "Symbol" "`symbolic " (Atom.Symbol "symbolic")
  ]

runTestsWithCLIArgs [] Array.empty testAtoms |> ignore