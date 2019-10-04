%{

open Ast

%}

/* Déclaration des terminaux */
%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token <string> FIELD
%token CASE WHEN THEN ELSE END EXTRACT
%token PLUS MINUS ASTERISK DIVIDE PIPE
%token LPAR RPAR COMMA DOT DATE CURRENT_DATE 
%token TRUE FALSE NOT NULL UNKNOWN IS
%token EQ TINY BIG TINYEQ BIGEQ DIFF
%token OR AND INNER JOIN UNION INTERSECT EXCEPT NATURAL
%token ALL AS BETWEEN BY CROSS
%token DISTINCT FOR FROM FULL 
%token LEFT LOWER ON UPPER WHERE
%token OUTER RIGHT SELECT SUBSTRING
%token TERM OPEN

/* Précédences (priorité + associativité) des terminaux */

%left FROM UNION EXCEPT
%left WHERE INTERSECT
%left COMMA ALL
%left JOIN ON CROSS INNER RIGHT LEFT OUTER FULL NATURAL
%left WHEN
%left OR
%left AND
%left NOT
%left IS
%left EQ TINY BIG TINYEQ BIGEQ DIFF
%left PIPE
%left DATE
%left PLUS MINUS
%left ASTERISK DIVIDE
%nonassoc UMINUS


/* Déclaration du non-terminal axiome (ici, ansyn) et du type de son attribut */
%type <Ast.bonus> ansyn
%start ansyn

%%

/* Déclaration de la grammaire avec les actions sémantiques */

ansyn:
	| TERM ansyn              { $2 }
	| bonus TERM              { $1 }
;
bonus: 
	| OPEN ID DOT ID          { newOpen $2 $4 }
	| query                   { newNormal $1 }
query:
	| SELECT projection FROM source                            { newFirstQuery $2 $4 }
	| SELECT ALL projection FROM source                        { newFirstQuery $3 $5 }
	| SELECT DISTINCT projection FROM source                   { newDistinctFirstQuery $3 $5 }
	| SELECT projection FROM source WHERE condition            { newSecondQuery $2 $4 $6 }
	| SELECT ALL projection FROM source WHERE condition        { newSecondQuery $3 $5 $7 }
	| SELECT DISTINCT projection FROM source WHERE condition   { newDistinctSecondQuery $3 $5 $7 }
	| query UNION query                                        { newUnion $1 $3 }
	| query UNION ALL query                                    { newUnionAll $1 $4 }
	| query EXCEPT query                                       { newExcept $1 $3 }
	| query EXCEPT ALL query                                   { newExceptAll $1 $4 }
	| query INTERSECT query                                    { newIntersect $1 $3 }
	| query INTERSECT ALL query                                { newIntersectAll $1 $4 }
;
projection:
	| ASTERISK                                         { newAsterisk }
	| column                                           { $1 }
;
column:
	| expression                                       { newColumn $1 }
	| expression AS ID                                 { newColumnAs $1 $3 }
	| column COMMA column                              { newColCoCol $1 $3 }
;
source:
	| ID                                               { newSrcID $1 }
	| LPAR query RPAR                                  { newSimpleQuery $2 }
	| source COMMA source                              { newSimpleSet $1 $3 }
	| source CROSS JOIN source                         { newCrossJoin $1 $4 }
	| source JOIN source ON condition                  { newJoin $1 $3 $5 }
	| source INNER JOIN source ON condition            { newInnerJoin $1 $4 $6 }
	| source LEFT JOIN source ON condition             { newLeftJoin $1 $4 $6 }
	| source LEFT OUTER JOIN source ON condition       { newLeftOuterJoin $1 $5 $7 }
	| source RIGHT JOIN source ON condition            { newRightJoin $1 $4 $6 }
	| source RIGHT OUTER JOIN source ON condition      { newRightOuterJoin $1 $5 $7 }
	| source FULL JOIN source ON condition             { newFullJoin $1 $4 $6 }
	| source FULL OUTER JOIN source ON condition       { newFullOuterJoin $1 $5 $7 }
	| source NATURAL JOIN source                       { newNatJoin $1 $4 }
	| source NATURAL INNER JOIN source                 { newNatInnerJoin $1 $5 }
	| source NATURAL LEFT JOIN source                  { newNatLeftJoin $1 $5 }
	| source NATURAL LEFT OUTER JOIN source            { newNatLeftOuterJoin $1 $6 }
	| source NATURAL RIGHT JOIN source                 { newNatRightJoin $1 $5 }
	| source NATURAL RIGHT OUTER JOIN source           { newNatRightOuterJoin $1 $6 }
	| source NATURAL FULL JOIN source                  { newNatFullJoin $1 $5 }
	| source NATURAL FULL OUTER JOIN source            { newNatFullOuterJoin $1 $6 }
;
condition:
	| LPAR condition RPAR                              { newCPar $2 }
	| condition AND condition                          { newAnd $1 $3 }
	| condition OR condition                           { newOr $1 $3 }
	| condition IS TRUE                                { newIsTrue $1 }
	| condition IS NOT TRUE                            { newIsNotTrue $1 }
	| condition IS FALSE                               { newIsFalse $1 }
	| condition IS NOT FALSE                           { newIsNotFalse $1 }
	| condition IS UNKNOWN                             { newIsUnknown $1 }
	| condition IS NOT UNKNOWN                         { newIsNotUnknown $1 }
	| expression EQ expression                         { newEq $1 $3 }
	| expression DIFF expression                       { newNeq $1 $3 }
	| expression TINY expression                       { newLt $1 $3 }
	| expression TINYEQ expression                     { newLe $1 $3 }
	| expression BIG expression                        { newGt $1 $3 }
	| expression BIGEQ expression                      { newGe $1 $3 }
	| expression BETWEEN expression AND expression     { newBtwn $1 $3 $5 }
	| expression NOT BETWEEN expression AND expression { newNotBtwn $1 $4 $6}
	| expression IS NULL                               { newIsNull $1 } 
	| expression IS NOT NULL                           { newIsNotNull $1 }
;
expression:
	| LPAR expression RPAR                             { newEPar $2 }
	| ID DOT ID                                        { newAttribute $1 $3 }
	| STRING                                           { newExprStr $1 }
	| INT                                              { newExprInt $1 }
	| FLOAT                                            { newExprFlo $1 }
	| expression PLUS expression                       { newExprPlus $1 $3 }
	| expression MINUS expression                      { newExprMinus $1 $3 }
	| expression ASTERISK expression                   { newExprAsterisk $1 $3 }
	| expression DIVIDE expression                     { newExprSlash $1 $3 }
	| MINUS expression %prec UMINUS                    { newExprUminus $2 }
	| expression PIPE expression                       { newPipe $1 $3 }
	| LOWER LPAR expression RPAR                       { newLower $3 }
	| UPPER LPAR expression RPAR                       { newUpper $3 }
	| SUBSTRING LPAR expression FROM expression FOR expression RPAR { newSubString $3 $5 $7 }
	| CASE expression WHEN casUn ELSE expression END   { newCaseOne $2 $4 $6 }	
	| CASE expression WHEN casUn END                   { newCaseOneBis $2 $4 }	
	| CASE WHEN casDeux ELSE expression END            { newCaseTwo $3 $5 }	
	| CASE WHEN casDeux END                            { newCaseTwoBis $3 }
	| CURRENT_DATE                                     { newCurrent }
	| DATE expression                                  { newDate $2 }
	| EXTRACT LPAR FIELD FROM expression RPAR          { newExtract $3 $5 }
;
casUn:
	| expression THEN expression                       { ($1,$3)::[] }
	| casUn WHEN casUn                                 { List.append $1 $3 }
;
casDeux:
	| condition THEN expression                        { ($1,$3)::[] }
	| casDeux WHEN casDeux                             { List.append $1 $3 }
;








