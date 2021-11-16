#ifndef __HW3_H
#define __HW3_H

#include <stdio.h>
#include <stdbool.h>

typedef enum {INT, REAL, STRING} LiteralType;

typedef struct LiteralValue {
	LiteralType returnType;
	union
	{
		int intValue;
		double realValue;
		char * strValue;
	};
} LiteralValue;

typedef struct AllAttributes {
        int lineNo;
        bool isConstant;
        bool mismatch;
	bool onlyLiteral;
        LiteralType returnType;
	LiteralValue value;
} AllAttributes;

#endif
