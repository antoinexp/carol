(* File lexer.mll *)
{
	open Parser        (* The type token is defined in parser.mli *)
	exception Eof
}

let digit = ['0'-'9']
let alnum = ['A'-'Z''a'-'z']
let variable = alnum (alnum|digit|'_')*
let lowoperator = ("+"|"-"|"%"|"++")
let highoperator =  ("*"|"/"|"**")
let logoperator = ("<"|"<="|">"|">="|"="|"<>"|"!=")

let space =  [' ' '\t' '\r' '\n']

rule token = parse
  [' ' '\t']     { token lexbuf } 
| ['\n' ]        { EOL }
| digit+ as s  	 { INT (int_of_string s) }
| (digit+ as s1) '.' (digit+ as s2)  { DEC (float_of_string (s1^"."^s2)) }
| "input"		 { INPUT }
| "output"		 { OUTPUT }
| "where"		 { WHERE }
| "fields"		 { FIELDS }
| "eval"		 { EVAL }
| "join"		 { JOIN }
| "true"         { BOOL true }
| "false"        { BOOL false }
| '|'            { PIPE }
| '['            { LBRACKET }
| ']'            { RBRACKET }
| '('            { LPAREN }
| ')'            { RPAREN }
| ','			 { COMMA }
| variable 		 { VARIABLE(Lexing.lexeme lexbuf) }
| lowoperator as op          { LOW_OPERATOR(op) }
| highoperator as op         { HIGH_OPERATOR(op) }
| logoperator as op          { LOG_OPERATOR(op) }
| eof            { raise Eof }