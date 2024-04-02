{
type token =
   Integer of int      
|  ArithmeticOP of char
|  Parenthesis of char
|  Assignment of char
|  Bool of string
|  BoolOP of char
|  CompOP of string
|  Keyword of string
|  StringConstant of string
|  StringOp of char
|  Identifier of string          
|  Comma of char
|  Semicolon of char    
|  UndefinedChar of char
|  Undefined of string
}

let nz_digit = ['1'-'9']
let digit = ['0'-'9']
let integer_constant = ['-'] ? digit +
let identifiers = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let whitespace = ' '|'\n'|'\r'|'\t'
let bool = "true" | "false"
let unallowedStrings = ['A'-'Z' '\''] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* | ['0'-'9']+ ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']+
let string_constant = '"'[^'"']*'"'
let keyword =  "if" | "then" | "else" | "let" | "pair" | "fst" | "snd" | "and" | "in" | "match" | "with" | "function" | "rec" 
let comp_op = "<=" | ">=" | "<" | ">" | "==" | "!="


rule read = parse
|    whitespace                                {read lexbuf}
|    ['+' '-' '*' '/' '%'] as x                {ArithmeticOP (x) :: read lexbuf}
|    ['&' '|' '!'] as x                        {BoolOP (x) :: read lexbuf}
|    ['='] as x                                {Assignment (x) :: read lexbuf}
|    ['(' ')'] as x                            {Parenthesis (x) :: read lexbuf}  
|    [';'] as x                                {Semicolon (x):: read lexbuf}
|    [','] as x                                {Comma (x):: read lexbuf}
|    comp_op as x                              {CompOP (x) :: read lexbuf}
|    ['^'] as x                                {StringOp (x) :: read lexbuf}
|    bool as x                                 {Bool (x) :: read lexbuf}
|    string_constant as x                      {StringConstant (x) :: read lexbuf}
|    keyword as x                              {Keyword (x) :: read lexbuf}
|    integer_constant as x                     {Integer (int_of_string x) :: read lexbuf}
|    unallowedStrings as x                     {Undefined (x) :: read lexbuf}
|    identifiers as x                          {Identifier (x) :: read lexbuf}
|    eof                                       {[]}
|    _ as x                                    {UndefinedChar (x) :: read lexbuf}

{
    let scan s = read (Lexing.from_string s)
}




(* 
scan "1234" ;;
scan "+ - * / %" ;;
scan "& | !" ;;
scan "=" ;;
scan "()" ;;
scan ";," ;;
scan "< <= > >= == !=" ;;
scan "^" ;;
scan "true false" ;;
scan "\"hello\" \"world\"" ;;
scan "if then else let pair fst snd and in match with function rec" ;;

scan "0123" ;;
scan "SomeString 9abc" ;;
scan "# @" ;;
scan "true & SomeString 123 #" ;;

scan "let i=-10 if i>103 then exit else (abs i)++";;
scan "let i=10 if i>=0 then fact*=i else i--";;
scan "";;
scan "let i=10000 if i<100 then exit else i div= 2";;
scan "if false then true else if true else false";;
scan "let a = 5 mod -3";;
scan "def a = bleh of int | string";;
scan "let a = +5 mod -3";;
scan "def a = bleh of int | string";;
scan "a \\/ b /\\ c";;
scan "+5";;
scan "+ 5";;
scan "52+5";;
scan "52 + 5";;
scan "4not3absT";;
scan "00123";;
scan "+0";;
scan "-0";;
scan "0";;
scan "not Not";;
scan "3#5";;
scan "GGGah";;
scan "Ax123";;
scan "AND";;
scan "?";;
scan "[]]";;
scan "";;
scan "\"\"";;


scan "999" ;;
scan "+=" ;;
scan "true || false" ;;
scan "varName" ;;
scan "'singleQuotes'" ;;
scan "123 + 456 - 789" ;;
scan "functionName(param1, param2)" ;;
scan "\"String with spaces\"" ;;
scan "let x = 5 in x * x" ;;
scan "(x > 5) && (y < 10)" ;;

scan "0x123" ;;
scan "Invalid-Identifier" ;;
scan "\"Unclosed string" ;;
scan "!!" ;;
scan ">>=" ;;
scan "@specialChar" ;;
scan "123abc" ;;
scan "@specialChar" ;;
scan "letx" ;;
 *)

