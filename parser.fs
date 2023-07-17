//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid simple C program.
//
// Adam Shaar
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // beginswith
  //
  let private beginswith (pattern: string) (literal: string) =
    // takes two strings and returns true if second string begins with the first string
    literal.StartsWith (pattern)
  //
  // matchTokenWithPattern
  //
  let private matchTokenWithPattern expected_pattern (tokens: string list) =
    // if first token in the list begins with the pattern, return the tail of the list
    let next_token = List.head tokens

    if beginswith expected_pattern next_token then
      List.tail tokens
    else
      failwith ("expecting " + expected_pattern + ", but found " + next_token)

  //
  // matchToken
  //
  let private matchToken expected_token (tokens: string list) =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)
      
  //
  // expr_op
  //
  let rec private expr_op tokens =
    // if first token in the list begins with an operator, return the tail of the list
    match List.head tokens with
    | "+" 
    | "-" 
    | "*" 
    | "/" 
    | "^" 
    | "<" 
    | "<=" 
    | ">" 
    | ">=" 
    | "==" 
    | "!=" -> List.tail tokens
    | next_token -> failwith ("expecting an expression operator, but found " + next_token)
  //
  // expr_value
  //
  let rec private expr_value tokens =
    // if first token is a type, return the tail of the list
    let token = List.head tokens
    match token with
    | _ when beginswith "identifier:" token 
           || beginswith "int_literal:" token 
           || beginswith "str_literal:" token 
           || token = "true" 
           || token = "false" -> List.tail tokens
    | _ -> failwith ("expecting identifier or literal, but found " + token)
  //
  // expr
  //
  let rec private expr tokens =
  // if first token is an expression value, function calls expr_op and itself to parse expression
    let T2 = expr_value tokens
    match List.head T2 with
    | "+" 
    | "-" 
    | "*" 
    | "/" 
    | "^" 
    | "<" 
    | "<=" 
    | ">" 
    | ">=" 
    | "==" 
    | "!=" -> let T3 = expr_op T2
              let T4 = expr T3
              T4
    | _ -> T2
  //
  // stmt
  //
  let rec private stmt tokens = 
  // if first token is ;, indentifier, int, cin, cout, or if, it calls its respective function
    match List.head tokens with
    | ";" -> empty tokens
    | _ when beginswith "identifier" (List.head tokens) -> assignment tokens
    | "int" -> vardecl tokens
    | "cin" -> input tokens
    | "cout" -> output tokens
    | "if" -> ifstmt tokens
    | _ -> failwith ("expecting statement, but found " + List.head tokens)
  //
  // then_part
  //
  and private then_part tokens =
  // representation of then part of if statement, calling stmt
    stmt tokens
  //
  // else_part
  //
  and private else_part tokens =
  // representation of else part of if statement, calling stmt or returning input list of tokens
    match List.head tokens with
    | "else" -> stmt (List.tail tokens)
    | _ -> tokens
  //
  // ifstmt
  //
  and private ifstmt tokens =
  // makes sure that each if-statement is in proper order
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let T4 = condition T3
    let T5 = matchToken ")" T4
    let T6 = then_part T5
    let T7 = else_part T6
    T7
  //
  // empty
  //
  and private empty tokens =
  // representation of semicolon
    let T2 = matchToken ";" tokens
    T2
  //
  // assignment
  //
  and private assignment tokens =
  // calls expr_value to parse side of =
    let T2 = expr_value tokens
    let T3 = matchToken "=" T2
    let T4 = expr T3
    let T5 = matchToken ";" T4
    T5
  //
  // vardecl
  // 
  and private vardecl tokens =
  // parses a variable declaration statement and returns the remaining tokens
    let T2 = matchToken "int" tokens
    let T3 = matchTokenWithPattern "identifier" T2
    let T4 = matchToken ";" T3
    T4
  //
  // input
  //
  and private input tokens =
  // parses an input statement and returns the remaining tokens
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    let T4 = matchTokenWithPattern "identifier" T3
    let T5 = matchToken ";" T4
    T5
  //
  // condition
  //
  and private condition tokens =
  // parses a condition expression and returns the remaining tokens
    expr tokens
  //
  // output_value
  //
  and private output_value tokens =
  // parses output_value in output statements and returns remaining tokens
    match List.head tokens with
    | "endl" -> List.tail tokens
    | _ -> expr_value tokens
  //
  // output
  //
  and private output tokens =
  // parses output statement and returns the remaining tokens
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = output_value T3
    let T5 = matchToken ";" T4
    T5
  //
  // morestmts
  //
  let rec private morestmts tokens =
  // parses the remaining statements in a code block and returns the remaining tokens
    match List.head tokens with
    | "}" | "$" -> tokens
    | _ -> let T2 = stmt tokens
           morestmts T2
  //
  // stmts
  //
  let rec private stmts tokens =
  // parses statements in a code block and returns the remaining tokens
    let T2 = stmt tokens
    let T3 = morestmts T2
    T3
  //
  // simpleC
  //
  let rec private simpleC tokens =
  // parses complete simple C program and returns the remaining tokens
   let T2 = matchToken "void" tokens;
   let T3 = matchToken "main" T2;
   let T4 = matchToken "(" T3;
   let T5 = matchToken ")" T4;
   let T6 = matchToken "{" T5;
   let T7 = stmts T6;
   let T8 = matchToken "}" T7;
   let T9 = matchToken "$" T8;
   T9

  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "success"
    with 
      | ex -> "syntax_error: " + ex.Message
