{
open Parser
exception Eof
}

(* Déclaration du dictionnaire (regexp -> terminal/token) *)
let digit      = ['0' - '9']
let alpha      = ['A'-'Z' 'a'-'z']


rule anlex = parse
	| [' ' '\t' '\n' '\r']     { anlex lexbuf (* Oubli des espacements et passages à la ligne *) }
	| (digit)+ as lxmi       { INT(int_of_string lxmi) }
	| (((digit)+ ['.']? (digit)*)| '.' (digit)+) (['e' 'E'] ['+' '-']? (digit)+)? as lxmf  { FLOAT(float_of_string lxmf) }
	| "--"                     { com lexbuf }
	| '+'                      { PLUS }
	| '*'                      { ASTERISK }
	| '-'                      { MINUS }
	| '/'                      { DIVIDE }
	| '('                      { LPAR }
	| ')'                      { RPAR }
	| ";;" | ";"               { TERM }
	|"ALL"                     { ALL }
	|"AND"                     { AND }
	|"AS"                      { AS }
	|"BETWEEN"                 { BETWEEN }
	|"BY"                      { BY }
	|"CASE"                    { CASE }
	|"CROSS"                   { CROSS }
	|"DISTINCT"                { DISTINCT }
	|"ELSE"                    { ELSE }
	|"END"                     { END }
	|"FALSE"                   { FALSE }
	|"FOR"                     { FOR }
	|"FROM"                    { FROM }
	|"FULL"                    { FULL }
	|"INNER"                   { INNER }
	|"IS"                      { IS }
	|"JOIN"                    { JOIN }
	|"LEFT"                    { LEFT }
	|"LOWER"                   { LOWER }
	|"NOT"                     { NOT }
	|"NULL"                    { NULL }
	|"ON"                      { ON }
	|"OR"                      { OR }
	|"OUTER"                   { OUTER }
	|"RIGHT"                   { RIGHT }
	|"SELECT"                  { SELECT }
	|"SUBSTRING"               { SUBSTRING }
	|"THEN"                    { THEN }
	|"TRUE"                    { TRUE }
	|"UNKNOWN"                 { UNKNOWN }
	|"UPPER"                   { UPPER }
	|"WHEN"                    { WHEN }
	|"WHERE"                   { WHERE }
	|"UNION"                   { UNION }
	|"EXCEPT"                  { EXCEPT }
	|"INTERSECT"               { INTERSECT }
	|"NATURAL"                 { NATURAL }
	|"DATE"                    { DATE }
	|"CURRENT_DATE"            { CURRENT_DATE }
	|"EXTRACT"                 { EXTRACT }
	|"YEAR" as str |"MONTH" as str |"DAY" as str    { FIELD(str) }
	|"OPEN"                    { OPEN }
	| "<="                     { TINYEQ }
	| "<"                      { TINY }
	| ">="                     { BIGEQ }
	| ">"                      { BIG }
	| "="                      { EQ }
	| "<>"                     { DIFF }
	| ","                      { COMMA }
	| "."                      { DOT }
	| "||"                     { PIPE }
	| alpha (alpha | digit)* as id1 { ID(id1) }
	| "\""               { ID(anyExceptQQuote "" lexbuf) }
	| "'"                { STRING(anyExceptQuoteOrQQ "" lexbuf) }
	| eof                      { raise Eof }
	| _  as lxm                { (* Pour tout autre caractère : message sur la sortie erreur + oubli *)
							   Printf.eprintf "Unknown character '%c': ignored\n" lxm; flush stderr;
							   anlex lexbuf
							 }

and com = parse
			| "\n"  { anlex lexbuf }
			| eof   { raise Eof }
			| _     { com lexbuf }
			
and anyExceptQQuote s = parse
	| '"'             { s }
	| _ as any        { anyExceptQQuote (s^( String.make 1 any )) lexbuf }
		
and anyExceptQuoteOrQQ s = parse
	| "''"             { anyExceptQuoteOrQQ (s^"'") lexbuf} 
	| '\''             { s }
	| _ as any         { anyExceptQuoteOrQQ (s^( String.make 1 any ))  lexbuf }
							 
							 
							 
							 
							 
							 
