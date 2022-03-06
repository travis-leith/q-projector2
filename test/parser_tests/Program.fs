open System

//runTestsWithCLIArgs [] Array.empty allTests |> ignore
//let sample = "
//  f:{[x;y] x = y};
  
//"
//sample |> Parser.parseFile |> printfn "%A"


let readFile path = IO.File.ReadAllText path



//IO.Directory.EnumerateFiles("Samples")
//  |> Seq.iter (fun path ->
//    path
//    |> readFile
//    |> ScopeParser.parseFile
//    |> printfn "%A"
//  )
//ScopeParser.parseCode "1+1" |> printfn "code: %A"
//ScopeParser.parseDefinition "a:1+1" |> printfn "definition: %A"
//ScopeParser.parseDefinition ".a.b:{[b]\n b + 1 }" |> printfn "definition2: %A"
//ScopeParser.parseFile ".a.b:{[b]\n b + 1 }" |> printfn "definition2: %A"
//ScopeParser.parseInvokaition "a+1" |> printfn "invokation: %A"
//ScopeParser.parseComment "//this is a comment: with a colon\n" |> printfn "comment: %A"
//ScopeParser.parseFunction "{[b] b+1}" |> printfn "function: %A"

ScopeParser.parseDefinition ".a.b:{[b]\n b + 1 }" |> printfn "definition2: %A"
ScopeParser.parseFile "if[99h<>type att;]" |> printfn "if: %A"

ScopeParser.parseFile "99h<>type att" |> printfn "type: %A"