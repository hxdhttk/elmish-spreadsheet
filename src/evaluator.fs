module Evaluator
open Parsec

// ----------------------------------------------------------------------------
// DOMAIN MODEL
// ----------------------------------------------------------------------------

type Position = char * int

type Expr = 
  | Reference of Position
  | Number of int
  | Binary of Expr * char * Expr

// ----------------------------------------------------------------------------
// PARSER
// ----------------------------------------------------------------------------

// Basics: operators (+, -, *, /), cell reference (e.g. A10), number (e.g. 123)
let operator = char '+' <|> char '-' <|> char '*' <|> char '/'
let reference = letter <*> integer |> map Reference
let number = integer |> map Number

// Nested operator uses need to be parethesized, for example (1 + (3 * 4)).
// <expr> is a binary operator without parentheses, number, reference or
// nested brackets, while <term> is always bracketed or primitive. We need
// to use `expr` recursively, which is handled via mutable slots.
let exprSetter, expr = slot ()
let brack = char '(' <*>> anySpace <*>> expr <<*> anySpace <<*> char ')'
let term = number <|> reference <|> brack 
let binary = term <<*> anySpace <*> operator <<*> anySpace <*> term |> map (fun ((l,op), r) -> Binary(l, op, r))
let exprAux = binary <|> term
exprSetter.Set exprAux

// Formula starts with `=` followed by expression
// Equation you can write in a cell is either number or a formula
let formula = char '=' <*>> anySpace <*>> expr
let equation = anySpace <*>> (formula <|> number) <<*> anySpace 

// Run the parser on a given input
let parse input = run equation input

// ----------------------------------------------------------------------------
// EVALUATOR
// ----------------------------------------------------------------------------

let binaryOpSome f (lhs: 'a option) (rhs: 'a option) : 'a option =
    match lhs, rhs with
    | Some(l), Some(r) -> Some(f l r)
    | _ -> None

let rec evaluate (cells: Map<Position, string>) (cellRefs: Set<Position>) expr = 
  match expr with
  | Number num -> 
      // Return the evluated number!
      Some num

  | Binary(l, op, r) -> 
      // Evaluate left and right recursively and then 
      // add/subtract/etc. them depending on the value of `op`
      match op with
      | '+' -> binaryOpSome (+) (evaluate cells cellRefs l) (evaluate cells cellRefs r)
      | '-' -> binaryOpSome (-) (evaluate cells cellRefs l) (evaluate cells cellRefs r)
      | '*' -> binaryOpSome (*) (evaluate cells cellRefs l) (evaluate cells cellRefs r)
      | '/' -> binaryOpSome (/) (evaluate cells cellRefs l) (evaluate cells cellRefs r)
      | _ -> None

  | Reference pos -> 
      // We need to evaluate value at `pos`. To do this,
      // get the expression in `cells` at `pos`, parse it and
      // call `evaluate` recursively to evaluate it. If the
      // `parse` function returns `None` then start by returning
      // -1 - we will fix this in the next step.
      // (This is harder than the two above cases!)
      match cellRefs |> Set.contains pos with
      | true -> None
      | false -> let valueOpt = cells |> Map.tryFind pos
                 match valueOpt with
                 | Some value -> let expressionOpt = parse value
                                 match expressionOpt with
                                 | Some expression -> evaluate cells (cellRefs |> Set.add pos) expression
                                 | None -> None
                 | None -> None