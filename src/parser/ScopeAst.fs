module ScopeAst

type IdentifierUse =
  |Definition of string * Scope option
  |Invocation of string
and Scope =
  {
    Paramaters:Set<string>
    IdentifierUsage:IdentifierUse list
  }