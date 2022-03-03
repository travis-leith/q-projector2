namespace qprojector
open System

type Identifier = Identifier of string

type Atom =
  |Number of string
  |String of string
  |Symbol of string
  |Date of DateOnly

type BinaryOperator = BinaryOperator of string //consider making a set of explicit operators

type BinaryExpression = 
  {
    LeftSide: Expression
    Operator: BinaryOperator
    RightSide: Expression
  }
and Function =
  {
    Parameters: Set<Identifier>
    Body: Expression
  }
and Expression = 
  |Atom of Atom
  |Assignment of Identifier * Expression
  |Identifier of Identifier
  |BinaryExpression of BinaryExpression

