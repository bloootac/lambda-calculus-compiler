#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

void main() {
	//try to open file
	const char* filename = "program.sk";
	FILE *fptr = fopen(filename, "r");
	
	//exit(0) = success, exit(1) = failure
	if (!fptr) exit(1);
	
	//tries to store file info in sb. need this to get the file size
	struct stat sb;
	if (stat(filename, &sb) == -1) exit(1);
	
	//allocate enough space to store input, then read from file
	char* input = malloc(sb.st_size);
	fread(input, sb.st_size, 1, fptr);
	
	//print input (sometimes adds an i at the end??)
	printf("%s", input);
	
	//close file, de-allocate memory
	fclose(fptr);
	free(input);
	
	exit(0); 
}