#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <stdbool.h>
#include "runc.h"


/*
TODO: clean everything up + add comments
*/

HeapComb* heap = NULL;
int* freeNodes = NULL;
int stackSize = 0;
int stackPtr = -1;
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
	
	printf("\n\nbuilding heap:\n");
	initHeap();
	buildHeap(root, 0);
	printHeap();
	
	//reduceK(2);
	printf("\nrun:\n");
	runComb();
	printHeap();
	printf("\n");
	heapToTree(0);
	
    //close file, de-allocate memory. TODO free the file data earlier?
    fclose(fptr);
	
    free(input);
    free(heap);
	free(freeNodes);
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
	
	stackSize = 10;
	freeNodes = (int*)malloc(stackSize * sizeof(HeapComb));
	
}

void buildHeap(Comb* comb, int index) {
	//printf("\nbuilding index %d\n", index); fflush(stdout);
	
	checkReallocHeap();
	
	HeapComb* ptr = heap + index;
	//printf("heap: %d\nptr: %d\n", heap, ptr);
	ptr->refs = 1;
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
			printf("\n%d: (+, %d, %d, %d)", i, h->left, h->right, h->refs); fflush(stdout);
		} else {
			printf("\n%d: (%s, %d, %d, %d)", i, h->val, h->left, h->right, h->refs); fflush(stdout);
		}
		
		//printf("\naddresses: h: %d, val: %d, left: %d, right: %d\n", h, &(h->val), &(h->left), &(h->right));
		
		h += 1; //this adds an offset of 16?? works fine ig
	}
	printf("\n");
}

// *************** run ***************


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

void reduceK(int index) {
	
	HeapComb* i = heap + index;
	HeapComb* a = heap + (heap + i->left)->right;
	
	if (a->left != -1) { 
		(heap + a->left)->refs += 1;
		(heap + a->right)->refs += 1;
	}
	
	decrementRefs(i->left);
	decrementRefs(i->right);
	editFrame(i, a->val, a->left, a->right, i->refs);

}

void reduceS(int index) {
	
	checkReallocHeap();
	
	HeapComb* i = heap + index; //+++Sfgx
	
	int fIndex = (heap + (heap + i->left)->left)->right;
	int gIndex = (heap + i->left)->right;
	int xIndex = i->right;
	
	(heap + fIndex)->refs += 1;
	(heap + gIndex)->refs += 1;
	(heap + xIndex)->refs += 1;
	
	decrementRefs(i->left);
	
	int leftLocation = findMemory();
	editFrame(heap + leftLocation, NULL, fIndex, xIndex, 1);
	int rightLocation = findMemory();
	editFrame(heap + rightLocation, NULL, gIndex, xIndex, 1);
	editFrame(i, NULL, leftLocation, rightLocation, i->refs);
	
	
	
	
}

void editFrame(HeapComb* i, char* val, int left, int right, int refs) {
	i->val = val;
	i->left = left;
	i->right = right;
	i->refs = refs;
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
		
		char c = findReductionDFS(indexPtr);
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

int findReductionDFS(int* indexPtr) {
	int stackSize = 50;
	int* combStack = (int*)malloc(stackSize * sizeof(int));
	
	int ptr = 0;
	int result = 0;
	int index;
	
	bool backtracking = false;
	int prevIndex;
	
	*combStack = 0;
	
	while (ptr != -1) {
		index = *(combStack + ptr);
		//not popping yet
		
		if (!backtracking) {
			//printf("checking index %d\n", index); fflush(stdout);
			if (matchK(index)) {
				*indexPtr = index;
				free(combStack);
				return 1;
			} else if (matchS(index)) {
				*indexPtr = index;
				free(combStack);
				return 2;
			} else {
			
				int left = (heap + index)->left;
				
				if (left != -1) {
					
					if (stackSize - 1 <= ptr) { 
						stackSize *= 2;
					    combStack = realloc(combStack, stackSize * sizeof(int));
					} 
					
					ptr++;
					*(combStack + ptr) = left;
				} else {
					//if (left != -1) result = -1; //mark not finished
					
					backtracking = true;
					ptr--;
					prevIndex = index;
		
				}
			
			}
		} else {
			//backtracking
			
			//check if prevIndex is left child. if so, push right, bt=false
			//else, prevI = i, pop, continue
			
			if ((heap + index)->left == prevIndex) {
				
				if (stackSize - 1 <= ptr) { 
					stackSize *= 2;
					combStack = realloc(combStack, stackSize * sizeof(int));
				} 
				
				
				ptr++;
				*(combStack + ptr) = (heap + index)->right;
				backtracking = false;
				
			} else {
				ptr--;
				prevIndex = index;
			}
			
		}
		
		
	}
	free(combStack);
	return result;
}


void logHeap() {
	
	FILE* fptr = fopen("logFile2.txt", "w");
	HeapComb* h = heap;
	int i;
	
	fprintf(fptr, "heapSize: %d\nheapLength: %d\n", heapSize, heapLength); fflush(fptr);
	
	for (i = 0; i < heapLength; i++) {

		if (h->val == NULL) {
			fprintf(fptr, "\n%d: (+, %d, %d, %d)", i, h->left, h->right, h->refs); fflush(fptr);
		} else {
			fprintf(fptr, "\n%d: (%s, %d, %d, %d)", i, h->val, h->left, h->right, h->refs); fflush(fptr);
		}
		
		//printf("\naddresses: h: %d, val: %d, left: %d, right: %d\n", h, &(h->val), &(h->left), &(h->right));
		
		h += 1; //this adds an offset of 16?? works fine ig
	}
	fprintf(fptr, "\n"); fflush(fptr);
	
	fclose(fptr);
	
}

void decrementRefs(int index) {
	int stackSize = 10;
	int* combStack = (int*)malloc(stackSize * sizeof(int));
	int ptr = 0;
	HeapComb* currentNode;
	
	*combStack = index;
	
	while (ptr >= 0) {
		//printf("decrementing ref for %d\n", *(combStack + ptr)); fflush(stdout);
		currentNode = heap + *(combStack + ptr);
		currentNode->refs -=1;
		ptr--;

		
		if (currentNode->refs == 0) {
			insertFreeNode(*(combStack + ptr + 1));
			
			if (currentNode->left != -1) {
				
				if (stackSize - 2 <= ptr) {
					stackSize *= 2;
					combStack = (int*)realloc(combStack, stackSize * sizeof(int));
				}
			
				ptr += 2;
				*(combStack + ptr - 1) = currentNode->left;
				*(combStack + ptr) = currentNode->right;
			}
			
			
		}
		
	}
	free(combStack);
	//printf("fin decr refs. stackPtr = %d\n", stackPtr); fflush(stdout);
	
}

void insertFreeNode(int index) {
	//realloc if needed
	
	if (stackSize - 2 <= stackPtr) {
		//printf("reallocating memory for freeNodes to %d\n", stackSize * 2); fflush(stdout);
		stackSize *= 2;
		freeNodes = (int*)realloc(freeNodes, stackSize * sizeof(int));
	}
	stackPtr++;
	
	*(freeNodes + stackPtr) = index;
	//printf("freeing %d. stackPtr = %d\n", *(freeNodes + stackPtr), stackPtr); fflush(stdout);
	
	
}

int findMemory() {
	//heapLength += 1;
	if (stackPtr == -1) {
		//realloc 
		checkReallocHeap();
		heapLength += 1;
		//printf("added a node to the heap at %d\n", heapLength - 1); fflush(stdout);
		return heapLength - 1;
		
		
	} else {
		//remove from stack
		//printf("overwriting location %d\n", *(freeNodes + stackPtr) ); fflush(stdout);
		stackPtr--;
		return *(freeNodes + stackPtr + 1);
		
	}
}

void printFreeNodes() {
	printf("free memory stack:\n");
	for (int i = 0; i <= stackPtr; i++) {
		printf("%d: %d\n", i, *(freeNodes + i)); fflush(stdout);
	}
	
}