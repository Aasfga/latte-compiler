#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void printInt(int v)
{
	printf("%d\n", v);
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
	int a;
	scanf("%d", &a);
	return a;
}

extern int __stringCompare(char *first, char *second)
{
  return strcmp(first, second);
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