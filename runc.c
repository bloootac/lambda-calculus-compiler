#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <stdbool.h>
#include "runc.h"


bool logComb = false;

HeapComb* heap = NULL;
int heapLength = 0;
int heapSize = 0;

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
    char* input = malloc(sb.st_size + 1);
    fread(input, sb.st_size, 1, fptr);
    input[sb.st_size] = '\0';
	
    //print input 
    printf("input:\n%s\n", input);
	
	//initialise struct
	Comb* root = malloc(sizeof(Comb));
	
	printf("tree:\n");
	strToTree(&(root->left), &(root->right), &(root->val), &(root->refs), input);
	printTree(root);
	
	fflush(stdout);
	
	// printf("\n\nbuilding heap:\n");
	initHeap();
	buildHeap(root, 0);
	// printHeap();
	
	//reduceK(2);
	printf("\nrun:\n");
	runComb();
	//printHeap();
	printf("\n");
	heapToTree(0);
	
    //close file, de-allocate memory. TODO de-allocate root too. and free the file data earlier?
    fclose(fptr);
	
    free(input);
    free(heap);
    exit(0); 
}

// *************** file -> tree ***************

void strToTree(Comb** left, Comb** right, char** val, int* refs, char* str) {
	
	if (!*str) {
		printf("called strToTree on null string"); // ಠ_ಠ
		exit(1);
	}
	
	char* splitIndex;
	
	if (*str == '+') {	
		//allocate memory for left + right terms
		*left = malloc(sizeof(Comb));
		*right = malloc(sizeof(Comb));
		*refs = 1;
		*val = NULL;
		
		//str = +AB. find index so we can separate A and B
		splitIndex = splitCombStr(str);
		
		//left = A
		strToTree(&((*left)->left), &((*left)->right), &((*left)->val), &((*left)->refs), str + 1);
		//right = B
		strToTree(&((*right)->left), &((*right)->right), &((*right)->val), &((*right)->refs), splitIndex);
		
	}
	else { 
		//str = S, K, or (var). find index to get full variable name
		splitIndex = getVar(str);
		
		//copy var to node 
		*val = malloc(splitIndex - str + 1); 
		memcpy(*val, str, splitIndex - str);
		*(*val + (splitIndex - str)) = '\0';
		
		//so we know this node is a leaf
		*left = NULL; *right = NULL;
		*refs = 1;
		
	}
	
}

//input: +AB. returns pointer to 1st char of B
char* splitCombStr(char *str) {
	int count = 0;

	while (*str) {
		if (*str == '+') count++;
		else if (*str == ')' || *str == 'S' || *str == 'K') count--;
		str++;
		if (count == 0) break;
	}
	return str;
	
}

//input: (var), S, or K. returns pointer to char after end of variable
char* getVar(char *str) {
	if (*str != '(') return str + 1;
	while (*str && *str != ')') str++;
	return str + 1;
}

void printTree(Comb *comb) {
	if (comb != NULL) {
		//printf("%d", comb->refs);
		if (comb->val != NULL)  {
			printf("%s", comb->val); 
		}
		else {
			printf("+");
			printTree(comb->left);
			printTree(comb->right);
		}	
	}
}

// *************** tree -> heap ***************

void initHeap() {
	heapSize = 200;
	heap = (HeapComb*)malloc(heapSize * sizeof(HeapComb));
	heapLength = 1;
}

void buildHeap(Comb* comb, int index) {
	//printf("\nbuilding index %d\n", index); fflush(stdout);
	
	checkReallocHeap();
	
	HeapComb* ptr = heap + index;
	//printf("heap: %d\nptr: %d\n", heap, ptr);
	
	if (comb->left == NULL && comb->right == NULL) {
		//printf("building %s\n", comb->val); fflush(stdout);
		ptr->val = comb->val;
		ptr->left = -1; //use -1 as null value instead of 0? so i don't get confused later 
		ptr->right = -1;
		//printf("heapSize: %d\nheapLength: %d\n", heapSize, heapLength); fflush(stdout);
	} else {
		//printf("heapSize: %d\nheapLength: %d\n", heapSize, heapLength); fflush(stdout);
		//printf("heap: %d\nptr: %d\n", heap, ptr);
		heapLength += 2;
		ptr->val = NULL;
		ptr->left = heapLength - 2;
		ptr->right = heapLength - 1;
		
		buildHeap(comb->left, heapLength - 2);
		buildHeap(comb->right, (heap + index)->right); //because heapLength might have changed now
		
	}
	//printf("****\n");
	
}

void checkReallocHeap() {
	if (heapLength >= heapSize - 2) {
		
		HeapComb* tempPtr = (HeapComb*)realloc(heap, heapSize * 2 * sizeof(HeapComb));
		
		if (tempPtr == NULL) {
			printf("realloc failed. exiting program"); fflush(stdout);
			free(heap);
			exit(1);
		} else {
			heap = tempPtr;
		}
		//printf("\nreallocated heap. size = %d\n", heapSize); fflush(stdout);
		//heap = (HeapComb*)realloc(heap, heapSize * 2 * sizeof(HeapComb));
		heapSize *= 2;
		//printf("reallocated heap. heapSize: %d\nheapLength: %d\n", heapSize, heapLength); fflush(stdout);
	}
}

void printHeap() {
	
	HeapComb* h = heap;
	int i;
	
	printf("heapSize: %d\nheapLength: %d\n", heapSize, heapLength);
	
	for (i = 0; i < heapLength; i++) {

		if (h->val == NULL) {
			printf("\n%d: (+, %d, %d)", i, h->left, h->right); fflush(stdout);
		} else {
			printf("\n%d: (%s, %d, %d)", i, h->val, h->left, h->right); fflush(stdout);
		}
		
		//printf("\naddresses: h: %d, val: %d, left: %d, right: %d\n", h, &(h->val), &(h->left), &(h->right));
		
		h += 1; //this adds an offset of 16?? works fine ig
	}
	printf("\n");
}

// *************** run ***************

//35985

bool matchK(int index) { 
	
	int i = (heap + index)->left;
	if (i != -1 && (heap + i)->left != -1) {
		
		char* str = (heap + (heap + i)->left)->val;
		
		return str != NULL && !strcmp(str, "K");
	}
	return false;
}

bool matchS(int index) {
	
	int i = (heap + index)->left;
	if (i != -1) {
		int j = (heap + i)->left;
		if (j != -1 && (heap + j)->left != -1) {
			char* str = (heap + (heap + j)->left)->val;
			return str != NULL && !strcmp(str, "S");
		}
	}
	return false;
}
 //TODO: print heap to file
void reduceK(int index) {
	HeapComb* i = heap + index;
	HeapComb* a = heap + (heap + i->left)->right;
	editFrame(i, a->val, a->left, a->right);
	
	//logHeap();
}


void reduceS(int index) {
	
	checkReallocHeap();
	
	HeapComb* i = heap + index;
	HeapComb* j = heap + i->left;
	
	heapLength += 2;
	HeapComb* end = heap + heapLength;
	
	editFrame(end - 2, NULL, (heap + j->left)->right, i->right);
	editFrame(end - 1, NULL, j->right, i->right);
	editFrame(i, NULL, heapLength - 2, heapLength - 1);
	
	
}

void editFrame(HeapComb* i, char* val, int left, int right) {
	i->val = val;
	i->left = left;
	i->right = right;
}

void heapToTree(int index) {
	HeapComb* i = heap + index;
	if (i->val == NULL) {
		printf("+");
		heapToTree(i->left);
		heapToTree(i->right);
	} else {
		printf("%s", i->val);
	}
}

void runComb() {
	bool reductionFound = true;
	int* indexPtr = malloc(sizeof(int));
	
	while (reductionFound) {
		reductionFound = false;
		
		//findReduction returns 0 if none, 1 if K, 2 if S. changes pointer to the location
		char c = findReduction(indexPtr);
		
		switch (c) {
			case 1:
				reductionFound = true;
				reduceK(*indexPtr);
				break;
			case 2:
				reductionFound = true;
				reduceS(*indexPtr);
				break;
		}
		//if 0, reductionFound left at false, loop finishes
		
	}
}

char findReduction(int* indexPtr) {
	//iterative dfs
	
	bool* notFinished = malloc(sizeof(bool));
	*notFinished = true;
	int depth = 0;
	char c = 0;
	while (c == 0 && *notFinished) {
		//printf("c = %d", c);
		*notFinished = false;
		depth += 5;
		c = findReductionHelper(indexPtr, 0, depth, notFinished);
	}
	return c;
	
}

char findReductionHelper(int* indexPtr, int index, int depth, bool* notFinished) {
	if (depth == 0) {
		//stop
		if ((heap + index)->left != -1) *notFinished = true;
		return 0;
	} 
	
	if ((heap + index)->left == -1) return 0;
	else if (matchK(index)) {
		*indexPtr = index;
		return 1;
	} else if (matchS(index)) {
		*indexPtr = index;
		return 2;
	} else {
		char b1 = findReductionHelper(indexPtr, (heap + index)->left, depth - 1, notFinished);
		if (b1 == 0) {
			char b2 = findReductionHelper(indexPtr, (heap + index)->right, depth - 1, notFinished);
			return b2;
		}
		return b1;
	}
	
	//otherwise ...
}

/* DFS at index. writes location of reduction to indexPtr
   returns 0 for no reduction, 1 for K-reduction, 2 for S-reduction */
// char findReduction(int* indexPtr, int index) {
	
	// if ((heap + index)->left == -1) return 0;
	// else if (matchK(index)) {
		// *indexPtr = index;
		// return 1;
	// } else if (matchS(index)) {
		// *indexPtr = index;
		// return 2;
	// } else {
		
		// char b1 = findReduction(indexPtr, (heap + index)->left);
		// if (b1 == 0) {
			// char b2 = findReduction(indexPtr, (heap + index)->right);
			// return b2;
		// }
		// return b1;
		
	// }
	
// }

void logHeap() {
	
	FILE* fptr = fopen("logFile2.txt", "w");
	HeapComb* h = heap;
	int i;
	
	fprintf(fptr, "heapSize: %d\nheapLength: %d\n", heapSize, heapLength); fflush(fptr);
	
	for (i = 0; i < heapLength; i++) {

		if (h->val == NULL) {
			fprintf(fptr, "\n%d: (+, %d, %d)", i, h->left, h->right); fflush(fptr);
		} else {
			fprintf(fptr, "\n%d: (%s, %d, %d)", i, h->val, h->left, h->right); fflush(fptr);
		}
		
		//printf("\naddresses: h: %d, val: %d, left: %d, right: %d\n", h, &(h->val), &(h->left), &(h->right));
		
		h += 1; //this adds an offset of 16?? works fine ig
	}
	fprintf(fptr, "\n"); fflush(fptr);
	
	fclose(fptr);
	
}
