module ScopeAst

type IdentifierUse =
  |Definition of string * Scope option
  |Invokation of string
and Scope =
  {
    Paramaters:Set<string>
    IdentifierUsage:IdentifierUse list
  }