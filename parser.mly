%{

open Lang ;;

let parse_error p = (* Called by the parser function on error *)
  print_endline p;
  flush stdout
;;

%}

%token <string> CONST
%token <string> VAR
%token <int> NAT   /* can only be a positive number */
%token LPAREN RPAREN LSQPAREN RSQPAREN
%token COMMA SEMICOLON COLON
%token SIMP RULES
%token TRACE STAR
%token ARROW
%token EOF

%start start
%type <Lang.rewr> start

%%

start: 
  RULES ruleList TRACE tracespec SIMP strategy termListAsTerm
  { Rewr($2, $4, $6, $7) }
;

id: 
   CONST
   {Const($1)}
 | NAT
   {term_of_nat($1)}
 | VAR
   {Var($1)}
;

constListCommaSep:
  CONST
  {[$1]}
| CONST COMMA constListCommaSep
  {$1::$3}
;

termList:
  term
  {[$1]}
| term termList
  {$1::$2}
;

termListAsTerm: 
  termList
  { term_of_term_list (List.rev $1) }
;

termListCommaSep:
  termListAsTerm
  {[$1]}
| termListAsTerm COMMA termListCommaSep
  {$1::$3}
;

termListCommaSepAsTerm:
  termListCommaSep
  { term_of_pair_term_list $1 }
;

term:
  id
  {$1}
| LPAREN termListCommaSepAsTerm RPAREN
  {$2}
;

ruleList:
  rule
  {[$1]}
| rule ruleList
  {$1::$2}
;

rule: 
  CONST COLON termListAsTerm ARROW termListAsTerm SEMICOLON
  {Rl($1, $3, $5)}
| termListAsTerm ARROW termListAsTerm SEMICOLON
  {Rl("", $1, $3)}
;

tracespec:
  STAR SEMICOLON
  {TraceAll}
| SEMICOLON
  {TraceSome []}
| constListCommaSep SEMICOLON
  {TraceSome $1}
;

strategy: LSQPAREN CONST RSQPAREN
  {$2}
;
