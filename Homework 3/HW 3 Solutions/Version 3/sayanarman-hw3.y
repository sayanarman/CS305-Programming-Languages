%{
	#include <stdio.h>
	#include <stdbool.h>
	#include <string.h>
	#include "sayanarman-hw3-attributes.h"

	void yyerror (const char *s) 
	{
	}

	extern int numberOfLines;

	void printResults (AllAttributes *);
	int  myRound(double);
	void insertNode(AllAttributes *);

	struct Node {
		AllAttributes * allAttributesPtr;
		struct Node * next;
	};

	struct Node * head = NULL;
%}

%union {
	int 		lineNo;
	LiteralValue  *	literalValuePtr;	
	AllAttributes *	allAttributesPtr;
}

%token tPRINT tGET tSET tFUNCTION tRETURN tIDENT tEQUALITY tIF tGT tLT tGEQ tLEQ tINC tDEC
%token <literalValuePtr>		 	tNUM
%token <literalValuePtr> 			tSTRING
%token <lineNo>					tADD
%token <lineNo>	                                tSUB
%token <lineNo>	                                tMUL
%token <lineNo>	                                tDIV
%type <allAttributesPtr> 			setStmt
%type <allAttributesPtr> 			if
%type <allAttributesPtr> 			print
%type <allAttributesPtr> 			expr
%type <allAttributesPtr> 			returnStmt
%type <allAttributesPtr> 			getExpr
%type <allAttributesPtr> 			operation
%type <allAttributesPtr> 			function
%type <allAttributesPtr> 			condition
%type <allAttributesPtr> 			exprList

%start prog

%%
prog:		'[' stmtlst ']' {
			struct Node * ptr;
			ptr = head;
			while (ptr != NULL)
			{
				printResults(ptr->allAttributesPtr);
				ptr = ptr->next;
			}
		}
;

stmtlst:	stmtlst stmt | 
;

stmt:		setStmt {insertNode($1);}
		| if {insertNode($1);}
		| print {insertNode($1);}
		| unaryOperation
		| expr {insertNode($1);}
		| returnStmt {insertNode($1);}
;

getExpr:	'[' tGET ',' tIDENT ',' '[' exprList ']' ']' {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = numberOfLines;
			allAttr->isConstant = false;
			allAttr->onlyLiteral = false;

			$$ = allAttr;			
		}
		| '[' tGET ',' tIDENT ',' '[' ']' ']' {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = numberOfLines;
			allAttr->isConstant = false;
			allAttr->onlyLiteral = false;

			$$ = allAttr;
		}
		| '[' tGET ',' tIDENT ']' {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = numberOfLines;
			allAttr->isConstant = false;
			allAttr->onlyLiteral = false;

			$$ = allAttr;
		}
;

setStmt:	'[' tSET ',' tIDENT ',' expr ']' {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = (*$6).lineNo;
			allAttr->isConstant = (*$6).isConstant;
			allAttr->mismatch = (*$6).mismatch;
			allAttr->onlyLiteral = (*$6).onlyLiteral;
			allAttr->returnType = (*$6).returnType;
			allAttr->value = (*$6).value;

			$$ = allAttr;			

			// MAYBE FREE $6 POINTER? ASK IT AND THINK IT
			//free($6);
		}
;

if:		'[' tIF ',' condition ',' '[' stmtlst ']' ']' {

		}
		| '[' tIF ',' condition ',' '[' stmtlst ']' '[' stmtlst ']' ']' {
		
		}
;

print:		'[' tPRINT ',' '[' expr ']' ']' {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = (*$5).lineNo;
			allAttr->isConstant = (*$5).isConstant;
			allAttr->mismatch = (*$5).mismatch;
			allAttr->onlyLiteral = (*$5).onlyLiteral;
			allAttr->returnType = (*$5).returnType;
			allAttr->value = (*$5).value;

			$$ = allAttr;
			
			// MAYBE FREE $5 POINTER? ASK IT AND THINK IT
			//free($5);
		}
;

operation:	'[' tADD ',' expr ',' expr ']' {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = $2;
			allAttr->isConstant = (*$4).isConstant && (*$6).isConstant;			


			if (allAttr->isConstant == true)
			{
				if(((*$4).returnType == INT || (*$4).returnType == REAL) && (*$6).returnType == STRING ||
			   	   ((*$6).returnType == INT || (*$6).returnType == REAL) && (*$4).returnType == STRING) allAttr->mismatch = true;
				else allAttr->mismatch = false;
			}
			else allAttr->mismatch = true;			

			allAttr->onlyLiteral = false;
			
			if (allAttr->mismatch == false)
			{
				if ((*$4).returnType == INT && (*$6).returnType == INT) {allAttr->returnType = INT; allAttr->value.intValue = (*$4).value.intValue + (*$6).value.intValue;}
                        	else if ((*$4).returnType == INT && (*$6).returnType == REAL) {allAttr->returnType = REAL; allAttr->value.realValue = (double) (*$4).value.intValue + (*$6).value.realValue;}
                        	else if ((*$4).returnType == REAL && (*$6).returnType == INT) {allAttr->returnType = REAL; allAttr->value.realValue = (*$4).value.realValue + (double) (*$6).value.intValue;}
                        	else if ((*$4).returnType == REAL && (*$6).returnType == REAL) {allAttr->returnType = REAL; allAttr->value.realValue = (*$4).value.realValue + (*$6).value.realValue;}
				else 
				{
					allAttr->returnType = STRING;
				
					// Concatenate Strings
					char str1[strlen((*$4).value.strValue) + strlen((*$6).value.strValue)], str2[strlen((*$6).value.strValue)];
					strcpy(str1, (*$4).value.strValue);
					strcpy(str2, (*$6).value.strValue);
					strcat(str1, str2);
					char * addResult = (char *) malloc(strlen(str1) + 1);
					strcpy(addResult, str1);
					allAttr->value.strValue = addResult;
				}
			}
			$$ = allAttr;
			
			// MAYBE FREE $4 and $6 POINTERS? ASK IT AND THINK IT
			//free($4); free($6);
		}
		| '[' tSUB ',' expr ',' expr ']' {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = $2;
                        allAttr->isConstant = (*$4).isConstant && (*$6).isConstant;
			
			if (allAttr->isConstant == true)
			{
                        	if ((*$4).returnType == STRING || (*$6).returnType == STRING) allAttr->mismatch = true;
				else allAttr->mismatch = false;
			}
			else allAttr->mismatch = true;
			
			allAttr->onlyLiteral = false;
			
			if (allAttr->mismatch == false)
			{
				if ((*$4).returnType == INT && (*$6).returnType == INT) {allAttr->returnType = INT; allAttr->value.intValue = (*$4).value.intValue - (*$6).value.intValue;}
				else if ((*$4).returnType == INT && (*$6).returnType == REAL) {allAttr->returnType = REAL; allAttr->value.realValue = (double) (*$4).value.intValue - (*$6).value.realValue;}
				else if ((*$4).returnType == REAL && (*$6).returnType == INT) {allAttr->returnType = REAL; allAttr->value.realValue = (*$4).value.realValue - (double) (*$6).value.intValue;}
				else {allAttr->returnType = REAL; allAttr->value.realValue = (*$4).value.realValue - (*$6).value.realValue;}
			}
			$$ = allAttr;

			// MAYBE FREE $4 and $6 POINTERS? ASK IT AND THINK IT
                        //free($4); free($6);
		}
		| '[' tMUL ',' expr ',' expr ']' {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = $2;
                       	allAttr->isConstant = (*$4).isConstant && (*$6).isConstant;
			
			if (allAttr->isConstant == true)
			{
				if ((*$4).returnType == STRING || (*$4).returnType == REAL && (*$6).returnType == STRING  ||
			    	    (*$4).returnType == INT && (*$4).value.intValue < 0 && (*$6).returnType == STRING) allAttr->mismatch = true;
				else allAttr->mismatch = false;
                        }
			else allAttr->mismatch = true;
			 
                        allAttr->onlyLiteral = false;
			
			if (allAttr->mismatch == false)
			{
				if ((*$4).returnType == INT && (*$6).returnType == INT) {allAttr->returnType = INT; allAttr->value.intValue = (*$4).value.intValue * (*$6).value.intValue;}
                        	else if ((*$4).returnType == INT && (*$6).returnType == REAL) {allAttr->returnType = REAL; allAttr->value.realValue = (double) (*$4).value.intValue * (*$6).value.realValue;}
                        	else if ((*$4).returnType == REAL && (*$6).returnType == INT) {allAttr->returnType = REAL; allAttr->value.realValue = (*$4).value.realValue * (double) (*$6).value.intValue;}
                        	else if ((*$4).returnType == REAL && (*$6).returnType == REAL) {allAttr->returnType = REAL; allAttr->value.realValue = (*$4).value.realValue * (*$6).value.realValue;}
                        	else   
                        	{
					allAttr->returnType = STRING;

					// Multiply String
					char str1[((*$4).value.intValue + 1) * strlen((*$6).value.strValue)], str2[strlen((*$6).value.strValue)];
					strcpy(str1, "");
					strcpy(str2, (*$6).value.strValue);
					
					int i = 0;
					for (; i < (*$4).value.intValue; i++) strcat(str1, str2);
					char * mulResult = (char *) malloc(strlen(str1) + 1);
					strcpy(mulResult, str1);
					allAttr->value.strValue = mulResult;
                        	}
			}
			$$ = allAttr;

			// MAYBE FREE $4 and $6 POINTERS? ASK IT AND THINK IT
                        //free($4); free($6);
		}
		| '[' tDIV ',' expr ',' expr ']' {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = $2;
                        allAttr->isConstant = (*$4).isConstant && (*$6).isConstant;
                       	
			if (allAttr->isConstant == true)
			{
				if ((*$4).returnType == STRING || (*$6).returnType == STRING) allAttr->mismatch = true;
                        	else allAttr->mismatch = false;
			}
			else allAttr->mismatch = true;			

                        allAttr->onlyLiteral = false;
			
			if (allAttr->mismatch == false)
			{
				if ((*$4).returnType == INT && (*$6).returnType == INT) {allAttr->returnType = INT; allAttr->value.intValue = (*$4).value.intValue / (*$6).value.intValue;}
                        	else if ((*$4).returnType == INT && (*$6).returnType == REAL) {allAttr->returnType = REAL; allAttr->value.realValue = (double) (*$4).value.intValue / (*$6).value.realValue;}
                        	else if ((*$4).returnType == REAL && (*$6).returnType == INT) {allAttr->returnType = REAL; allAttr->value.realValue = (*$4).value.realValue / (double) (*$6).value.intValue;}
                        	else {allAttr->returnType = REAL; allAttr->value.realValue = (*$4).value.realValue / (*$6).value.realValue;}
			}
			$$ = allAttr;

			// MAYBE FREE $4 and $6 POINTERS? ASK IT AND THINK IT
                        //free($4); free($6);
		}
;	

unaryOperation: '[' tINC ',' tIDENT ']'
		| '[' tDEC ',' tIDENT ']'
;

expr:		tNUM {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = numberOfLines;
			allAttr->isConstant = true;
			allAttr->mismatch = false;
			allAttr->onlyLiteral = true;
			allAttr->returnType = (*$1).returnType;
			if ((*$1).returnType == INT) allAttr->value.intValue = (*$1).intValue;
			else allAttr->value.realValue = (*$1).realValue;

			$$ = allAttr;

			// MAYBE FREE $1 POINTER? ASK IT AND THINK IT
			//free($1);
		} 
		| tSTRING {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = numberOfLines;
			allAttr->isConstant = true;
			allAttr->mismatch = false;
			allAttr->onlyLiteral = true;
			allAttr->returnType = (*$1).returnType;
			allAttr->value.strValue = (*$1).strValue;
			
			$$ = allAttr;

			// MAYBE FREE $1 POINTER? ASK IT AND THINK IT
			//free($1);
		}
		| getExpr {
			$$ = $1;

			// MAYBE MALLOC AS tNUM and tSTRING? THINK IT
		} 
		| function {
			$$ = $1;
			
			// MAYBE MALLOC AS tNUM and tSTRING? THINK IT
		} 
		| operation {
                        $$ = $1;
			
			// MAYBE MALLOC AS tNUM and tSTRING? THINK IT
		}
		| condition {
                        $$ = $1;

			// MAYBE MALLOC AS tNUM and tSTRING? THINK IT
		}
;

function:	'[' tFUNCTION ',' '[' parametersList ']' ',' '[' stmtlst ']' ']' {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = numberOfLines;
			
			$$ = allAttr;
		}
		| '[' tFUNCTION ',' '[' ']' ',' '[' stmtlst ']' ']' {
			AllAttributes  * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = numberOfLines;
			
			$$ = allAttr;
		}
;

condition:	'[' tEQUALITY ',' expr ',' expr ']' {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = numberOfLines;

			$$ = allAttr;
			
			insertNode($4);
			insertNode($6);
		}
		| '[' tGT ',' expr ',' expr ']' {
			AllAttributes *	allAttr	= (AllAttributes *) malloc(sizeof(AllAttributes));
                        allAttr->lineNo	= numberOfLines;

                        $$ = allAttr;
			
			insertNode($4);
                        insertNode($6);
		}
		| '[' tLT ',' expr ',' expr ']' {
			AllAttributes *	allAttr	= (AllAttributes *) malloc(sizeof(AllAttributes));
                        allAttr->lineNo	= numberOfLines;

                        $$ = allAttr;
			
			insertNode($4);
                        insertNode($6);
		}
		| '[' tGEQ ',' expr ',' expr ']' {
			AllAttributes *	allAttr	= (AllAttributes *) malloc(sizeof(AllAttributes));
                        allAttr->lineNo	= numberOfLines;

                        $$ = allAttr;
			
			insertNode($4);
                        insertNode($6);
		}
		| '[' tLEQ ',' expr ',' expr ']' {
			AllAttributes *	allAttr	= (AllAttributes *) malloc(sizeof(AllAttributes));
                        allAttr->lineNo	= numberOfLines;

                        $$ = allAttr;
			
			insertNode($4);
                        insertNode($6);
		}
;

returnStmt:	'[' tRETURN ',' expr ']' {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = (*$4).lineNo;
			allAttr->isConstant = (*$4).isConstant;
			allAttr->mismatch = (*$4).mismatch;
			allAttr->onlyLiteral = (*$4).onlyLiteral;			
			allAttr->returnType = (*$4).returnType;
			allAttr->value = (*$4).value;
                        
			$$ = allAttr;
			
			// MAYBE FREE $4 POINTER? ASK IT AND THINK IT
			//free($4);
		}
		| '[' tRETURN ']' {
			AllAttributes * allAttr = (AllAttributes *) malloc(sizeof(AllAttributes));
			allAttr->lineNo = numberOfLines;
			allAttr->isConstant = false;
			allAttr->mismatch = false;		

			$$ = allAttr;
		}
;

parametersList: parametersList ',' tIDENT | tIDENT
;

exprList:	exprList ',' expr {insertNode($3);} 
		| expr {insertNode($1);}
;

%%

void insertNode(AllAttributes * allAtt)
{
	if (head == NULL)
	{
		struct Node * new_node = (struct Node*) malloc(sizeof(struct Node));
		new_node->allAttributesPtr = allAtt;
		new_node->next = NULL;
		head = new_node;
	}

	else
	{
		struct Node * ptr;
		ptr = head; 
		while (ptr->next != NULL) ptr = ptr->next;
		
		struct Node * new_node = (struct Node*) malloc(sizeof(struct Node));
		new_node->allAttributesPtr = allAtt;
		new_node->next = NULL;
		ptr->next = new_node;
	}
}

int myRound(double x)
{
	if (x < 0.0)	return (int)(x - 0.5);
	else		return (int)(x + 0.5);
}

void printResults(AllAttributes * allAtt)
{
	if ((*allAtt).onlyLiteral == true || (*allAtt).isConstant == false)
	{
		//DO NOTHING (PRINT NOTHING)
		printf("");
	}
	
	else if ((*allAtt).mismatch == true)
	{
		printf("Type mismatch on %d\n", (*allAtt).lineNo);
		//free(allAtt);
	}

	else if ((*allAtt).returnType == INT)
	{
		printf("Result of expression on %d is (%d)\n", (*allAtt).lineNo, (*allAtt).value.intValue);
		//free(allAtt);
	}
	
	else if ((*allAtt).returnType == REAL)
	{
		double realValue = (*allAtt).value.realValue;
		realValue = (double) myRound(realValue * 10)/10;

		printf("Result of expression on %d is (%.1f)\n", (*allAtt).lineNo, realValue);
		//free(allAtt);
	}

	else if ((*allAtt).returnType == STRING)
	{
		printf("Result of expression on %d is (%s)\n", (*allAtt).lineNo, (*allAtt).value.strValue);
		//free(allAtt);
	}

	else
	{
		printf("PROGRAM HAS A BUG ON PRINT STATEMENT WHICH ENTERS ELSE!!!\n");       
	}
}

int main ()
{
	if (yyparse()) {
		// parse error
		printf("ERROR\n");
		return 1;
	}
	else {
		// successful parsing
		//printf("OK\n");
		return 0;
	}
}
