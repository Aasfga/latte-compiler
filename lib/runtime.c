#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define integer long long int

struct array
{
	integer *actualArray;
	integer size;
};

extern int debugGcsePrintInt(integer x)
{
	printf("%lld\n", x);
	return x;
}

extern void printInt(integer v)
{
	printf("%lld\n", v);
}

extern void printString(char* v)
{
  printf("%s\n", v);
}

extern void error()
{
	exit(-1);
}

extern int readInt()
{
	integer a;
	scanf("%lld", &a);
	return a;
}

extern void* __createObject(integer size) 
{
	return calloc(size, sizeof(integer));
}

extern struct array* __createArray(integer size)
{
	struct array* arrayObject = calloc(2, sizeof(struct array));	
	arrayObject->actualArray = calloc(size, sizeof(integer));
	arrayObject->size = size;
	return arrayObject;
}

extern int __stringCompare(char *first, char *second)
{
  return strcmp(first, second);
}

extern int __pointerCompare(integer first, integer second)
{
	return first - second;
}

extern char* __stringConcat(char* first, char* second)
{
	int firstLength = strlen(first);
	int secondLength = strlen(second);
	int newLength = firstLength + secondLength + 1;
	char* newString = malloc(sizeof(char) * newLength);
	memcpy(newString, first, firstLength);
	memcpy(newString + firstLength, second, secondLength);
	newString[newLength - 1] = 0;
	return newString;
}

extern char* readString()
{
  char *newString = malloc(sizeof(char) * 1001);
  scanf("%1000s", newString);
	return newString;
}