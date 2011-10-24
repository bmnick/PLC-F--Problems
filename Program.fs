// PLC Project 2 GIANT BLOB OF CODE!
//
// Ben Nicholas

#light 

//open FsCheck // Get a testing library in for simplifying junk 

// ****************************************************************************
// BEGIN PROBLEM 1
// ****************************************************************************

// EUCLIDEAN ALGORITHM, DO YOU SPEAK IT?
let rec gcd a b = 
    match b with
    | 0 -> a
    | _ -> gcd b (a % b)

// ****************************************************************************
// BEGIN PROBLEM 2
// ****************************************************************************
open System

let dateFormatter (date:DateTime) =
    date.ToString("dddd, MMMM dd, yyyy")

let convertDate dateString =
    System.DateTime.ParseExact(dateString, "MM/dd/yyyy", null)
    |> dateFormatter

// ****************************************************************************
// BEGIN PROBLEM 3
// ****************************************************************************
// Create a count tuple from an individual character (embarassingly parallelizable)
let createTuple = function
    | 'a' -> (1,0,0,0,0)
    | 'e' -> (0,1,0,0,0)
    | 'i' -> (0,0,1,0,0)
    | 'o' -> (0,0,0,1,0)
    | 'u' -> (0,0,0,0,1)
    | _ -> (0,0,0,0,0)

// Add all entries in a 5-tuple (embarassingly parallelizable)
let addTuples (accACount, accECount, accICount, accOCount, accUCount) (aCount, eCount, iCount, oCount, uCount) = 
    (accACount + aCount, accECount + eCount, accICount + iCount, accOCount + oCount, accUCount + uCount)

// Count vowels in a string
let countVowels (inputString:string) = 
    inputString.ToCharArray() // non parallizable
    |> Array.map createTuple // can be replaced with a parallel map
    |> Array.fold addTuples (0,0,0,0,0) // can be replaced with a parallel fold/reduce

// ****************************************************************************
// BEGIN PROBLEM 4
// ****************************************************************************

let rec transpose = function
    | (_::_)::_ as matrix -> // Match against a list of lists of ints
        // pull off all the heads, make them a list, then append the transpose of the tails of the matrix
        List.map List.head matrix :: transpose (List.map List.tail matrix)
    | _ -> [] // Return empty matrix when not a list of lists

// ****************************************************************************
// BEGIN PROBLEM 5
// ****************************************************************************

open System

let (|Value|Variable|Operator|OpenParen|CloseParen|WhiteSpace|BadInput|) = function
    | '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' -> Value
    | 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z' -> Variable
    | '+'|'-'|'/'|'*'|'^' -> Operator
    | '(' -> OpenParen
    | ')' -> CloseParen
    | ' ' -> WhiteSpace
    | _ -> BadInput

let rec shunting output op_stack = function
    | WhiteSpace::rest -> shunting output op_stack rest
    | Value as v::rest -> shunting (output + v.ToString() + " ") op_stack rest
    | Variable as v::rest -> shunting (output + v.ToString() + " ") op_stack rest
    | Operator as o::rest -> shunting_operator output rest o op_stack
    | OpenParen::rest -> shunting output ('('::op_stack) rest
    | CloseParen::rest -> flush_to_open_paren output rest op_stack
    | [] -> flush_op_stack output op_stack
    | _ -> "Bad input"

and shunting_operator output input_string operator (op_stack:char list) =
    Console.WriteLine(operator)
    Console.WriteLine(op_stack)
    match op_stack with
    | '+'::rest -> match operator with
                        | '+'|'-' -> shunting_operator (output + "+") input_string operator rest
                        | '/'|'*'|'^' -> shunting output (operator::op_stack) input_string
                        | _ -> "Input error"
    | '-'::rest -> match operator with
                        | '+'|'-' -> shunting_operator (output + "-") input_string operator rest
                        | '/'|'*'|'^' -> shunting output (operator::op_stack) input_string
                        | _ -> "Input error"
    | '/'::rest -> match operator with
                        | '+'|'-'|'/'|'*' -> shunting_operator (output + "/") input_string operator rest
                        | '^' -> shunting output (operator::op_stack) input_string
                        | _ -> "Input error"
    | '*'::rest -> match operator with
                        | '+'|'-'|'/'|'*' -> shunting_operator (output + "*") input_string operator rest 
                        | '^' -> shunting output (operator::op_stack) input_string
                        | _ -> "Input error"
    | '^'::rest -> match operator with
                        | '+'|'-'|'/'|'*' -> shunting_operator (output + "^") input_string operator rest
                        | '^' -> shunting output (operator::op_stack) input_string
                        | _ -> "Input error"
    | '('::rest -> shunting output (operator::op_stack) input_string
    | [] -> shunting output (operator::op_stack) input_string 
    | _ -> "Input error"

and flush_to_open_paren output input = function
    | '('::rest -> shunting output rest input
    | _ as op::rest -> flush_to_open_paren (output + (op.ToString()) + " ") input rest
    | [] -> "Unmatched Parentheses"

and flush_op_stack (output:string) = function
    | '('::rest -> "Unmatched parentheses"
    | _ as op::rest -> flush_op_stack (output + (op.ToString()) + " ") rest
    | [] -> output

let in2post (input_string:string) = shunting "" [] (input_string.ToCharArray() |> List.ofArray)

// ****************************************************************************
// BEGIN TESTS
// ****************************************************************************

// Test helper method for gcd, checking that the result divides both numbers
let gcdDividesBothParams a b = 
    match a with
    | 0 -> true
    | _ -> match b with
             | 0-> true
             | _ -> a % (gcd a b) = 0 && b % (gcd a b) = 0

// Validate that gcd passes first test
//Check.Quick gcdDividesBothParams

// *****************************************************************************
// BEGIN STUPID WINDOWS HACKERY
// *****************************************************************************
open System
ignore (Console.Read()) // Pause to check test results