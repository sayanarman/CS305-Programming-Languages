%{
	#include<stdio.h>
	
	void yyerror(const char *s)
	{
		// called by yyparse on error
		printf("%s\n", s);
	}
%}


%token	tSTRING
%token	tNUM
%token	tPRINT
%token	tGET
%token	tSET
%token	tFUNCTION
%token	tRETURN
%token	tIDENT
%token	tEQUALITY
%token	tIF
%token	tGT
%token	tLT
%token	tGEQ
%token	tLEQ
%token	tINC
%token	tDEC

 
%start prog

%%

prog:		'[' stmtlst ']'
		;

stmtlst:	 
		| stmtlst stmt
		;

stmt:		set
		| if
		| print
		| increment
		| decrement
		| return
		| expr  
		;

set:		'[' tSET ',' tIDENT ',' expr ']'
		;

if:		'[' tIF ',' cond ',' then else ']'
		| '[' tIF ',' cond ',' then ']'
		;

print:		'[' tPRINT ',' '[' expr ']' ']'
		;

increment:	'[' tINC ',' tIDENT ']' 
		;

decrement:	'[' tDEC ',' tIDENT ']'
		;

return:		'[' tRETURN ',' expr ']'
		| '[' tRETURN ']'
		;

expr:		tNUM
		| tSTRING
		| get
		| func
		| operator
		| cond
		;

cond:		'[' tLEQ ',' expr ',' expr ']'
		| '[' tGEQ ',' expr ',' expr ']'
		| '[' tEQUALITY ',' expr ',' expr ']'
		| '[' tGT ',' expr ',' expr ']'
		| '[' tLT ',' expr ',' expr ']'
 		;

then:		'[' stmtlst  ']'
		;

else:		'[' stmtlst ']'
		;

get:		'[' tGET ',' tIDENT ']'
		| '[' tGET ',' tIDENT ',' '[' ']' ']'
		| '[' tGET ',' tIDENT ',' '[' exprlst ']' ']'
		;

func:		'[' tFUNCTION ',' '[' ']' ',' '[' stmtlst ']' ']'
		| '[' tFUNCTION ',' '[' paramlst ']' ',' '[' stmtlst ']' ']' 
		;

operator:	'[' '"' '+' '"' ',' expr ',' expr ']'
		| '[' '"' '-' '"' ',' expr ',' expr ']'
		| '[' '"' '*' '"' ',' expr ',' expr ']'
		| '[' '"' '/' '"' ',' expr ',' expr ']'
		;

exprlst:	expr ',' exprlst
		| expr
		;

paramlst:	paramlst ',' tIDENT
		| tIDENT
		;

%%
int main()
{
	if (yyparse())
	{
		// parse error
		printf("ERROR\n");
		return 1;
	}
	else
	{
		// successful parsing
		printf("OK\n");
		return 0;
	}
}
