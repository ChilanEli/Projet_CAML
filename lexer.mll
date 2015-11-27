{

  open Lexing
  open Parser
  open Lang
  exception Lexerror


  let pos lexbuf = (lexeme_start lexbuf, lexeme_end lexbuf)

  let advance_line_pos pos =
    { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum; }

  let advance_line lexbuf =
    lexbuf.lex_curr_p <- advance_line_pos lexbuf.lex_curr_p

}

let alph_l =           ['a'-'z']
let alph_u =           ['A'-'Z']
let alph =           ['_''a'-'z''A'-'Z']
let num  =           ['0'-'9'] 
let decimal	=	'0'|(['1'-'9']['0'-'9']*)
let ml_comment = '/' '*' [^'*']* ('*' (('*'*)|([^'*''/'][^'*']*)))* '*' '/'
let sl_comment = '/' '/' ([^'@''\n'] [^'\n']*)? '\n' 
let stringLiteral='\"' [^'\"']* ('\\' '\"' [^'\"']*)* '\"'
let charLiteral='\'' [^'\''] '\''

rule token = parse
 [' ' '\t']
    { token lexbuf }    (* white space: recursive call of lexer *)
|'\n'
    {advance_line lexbuf; token lexbuf }    (* white space: recursive call of lexer *)
| ml_comment
    { token lexbuf }    (* comment --> ignore *)
| sl_comment
    { token lexbuf }    (* comment --> ignore *)
| decimal  as i	  { NAT (int_of_string i)}

| '('  { LPAREN }
| ')'  { RPAREN }
| '['  { LSQPAREN }
| ']'  { RSQPAREN }
| ','  { COMMA }
| ';'  { SEMICOLON }
| ':'  { COLON }
| '*'  { STAR }
| "->"         { ARROW }
| "rules"       { RULES }
| "simp"      { SIMP }
| "trace"      { TRACE }

| eof          { EOF }

| alph_l(alph|num)* as i  {CONST i}
| alph_u(alph|num)* as i  {VAR i}

| _  {Printf.printf "ERROR: unrecogized symbol '%s'\n" (Lexing.lexeme lexbuf);
      raise Lexerror }

and
    ruleTail acc = parse
| eof { acc }
| _* as str { ruleTail (acc ^ str) lexbuf }
