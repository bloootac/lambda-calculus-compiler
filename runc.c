
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <stdbool.h>
#include "temp.h"
#include "runc.h"

HeapComb* heap = NULL;
int heapLength = 0;
int heapSize = 0;

int* freeStack = NULL;
int freeStackSize = 0;
int freeStackPtr = 0;

int* searchStack = NULL;
int searchStackSize = 0;
int searchStackPtr = 0;


/*
TODO:
fix segfault with global stack. happens while decrementing refs ...
*/

void main(int argc, char *argv[]) {
    
	//old version:
    // const char* filename = "program.sk";
    // FILE *fptr = fopen(filename, "r");
    // if (!fptr) exit(1); //0 = failure, 1 = success
	
	// //get file size
    // struct stat sb;
    // if (stat(filename, &sb) == -1) exit(0);
    // char* input = malloc(sb.st_size + 1);
	
	// //copy file contents
	// readFile(fptr, input);
    // fclose(fptr);
	
	//temp_sk is declared in temp.h, which is made just before compiling this runtime
	//it contains the contents of the .sk file
	char* input = temp_sk;
	
	//run the file
    //printf("input:\n%s\n", input);
	runFile(input);
	
	//finish
	//free(input);
    exit(0); 
}

//copy file contents into input array
void readFile(FILE* fptr, char* input) {
	int i = 0;
	int c;
	while ((c = fgetc(fptr)) != EOF) {
		*(input + i) = c;
		i++;
	}
	*(input + i) == '\0';
}

//run all lines in a file
void runFile(char* input) {
	char* line = input;
	while (line) {
		
		//create Comb tree from input...
		Comb* root = malloc(sizeof(Comb));
		strToTree(&(root->left), &(root->right), &(root->val), line);
		
		//printf("\n\ntree:\n");
		//printTree(root);
		//fflush(stdout);
		
		//..and build heap from tree
		initStacks();
		buildHeap(root, 0);
		
		//reduce term
		//printf("\nrun:\n");
		runComb();
		printHeapTree(0);

		//de-allocate everything
		freeCombTree(root);
		free(heap);
		free(freeStack);
		free(searchStack);
		
		//move on to next line of input
		line = strchr(line, '\n');
		if (line) line += 1;
		printf("\n");
		
	}
}



// *************** Comb tree ***************

//input: addresses of a tree's left, right, and val pointers, and a pointer to a tree string. build Comb tree from input
void strToTree(Comb** left, Comb** right, char** val, char* str) {
	
	if (!*str) {
		printf("called strToTree on null string"); // ಠ_ಠ
		exit(1);
	}
	
	char* splitIndex;
	
	if (*str == '+') {	
		//allocate memory for left + right terms
		*left = malloc(sizeof(Comb));
		*right = malloc(sizeof(Comb));
		*val = NULL;
		
		//str = +AB. find index so we can separate A and B
		splitIndex = splitCombStr(str);
		
		//left = A
		strToTree(&((*left)->left), &((*left)->right), &((*left)->val), str + 1);
		//right = B
		strToTree(&((*right)->left), &((*right)->right), &((*right)->val), splitIndex);
		
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
		
	}
	
}

//input: +AB. return pointer to 1st char of B
char* splitCombStr(char *str) {
	int count = 0;

	while (*str) {
		if (*str == '+') count++;
		else if (*str == ')' || *str == 'S' || *str == 'K' || *str == 'I') count--;
		str++;
		if (count == 0) break;
	}
	return str;
	
}

//input: (var), S, or K. return pointer to char after end of variable
char* getVar(char *str) {
	if (*str != '(') return str + 1;
	while (*str && *str != ')') str++;
	return str + 1;
}

void printTree(Comb *comb) {
	if (comb != NULL) {
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

//deallocate memory used for the Comb tree and variable names
void freeCombTree(Comb* ptr) {
	if (ptr->left != NULL) {
		freeCombTree(ptr->left);
		freeCombTree(ptr->right);
	} else {
		free(ptr->val);
	}
	free(ptr);
}



// *************** heap ***************

//create arrays for the heap, the stack of free heap locations, and the DFS search stack
void initStacks() {
	heapSize = 200;
	heap = (HeapComb*)malloc(heapSize * sizeof(HeapComb));
	heapLength = 1;
	
	freeStackSize = 10;
	freeStack = (int*)malloc(freeStackSize * sizeof(HeapComb));
	freeStackPtr = -1;
	
	searchStackSize = 10;
	searchStack = (int*)malloc(searchStackSize * sizeof(int));
	searchStackPtr = 0;
	*searchStack = 0;
	
}

//build heap from Comb tree
void buildHeap(Comb* comb, int index) {
	
	checkReallocHeap();
	HeapComb* ptr = heap + index;

	ptr->refs = 1;
	if (comb->left == NULL && comb->right == NULL) {
		//set values for variable node
		ptr->val = comb->val;
		ptr->left = -1;
		ptr->right = -1;

	} else {
		//make space for left and right children
		heapLength += 2;
		ptr->val = NULL;
		
		//link this node to its children on the stack
		ptr->left = heapLength - 2;
		ptr->right = heapLength - 1;
		
		//add children to heap
		buildHeap(comb->left, heapLength - 2);
		buildHeap(comb->right, (heap + index)->right);
		
	}
	
}

//request twice as much memory for heap, exit program if unsuccessful
void checkReallocHeap() {
	if (heapLength >= heapSize - 2) {
		heapSize *= 2;
		HeapComb* tempPtr = (HeapComb*)realloc(heap, heapSize * sizeof(HeapComb));
		if (tempPtr == NULL) {
			reallocFail(heap);
		} else {
			heap = tempPtr;
		}
	}
}

//overwrite a node on a heap
void editFrame(HeapComb* i, char* val, int left, int right, int refs) {
	i->val = val;
	i->left = left;
	i->right = right;
	i->refs = refs;
}

//decrement the reference counts of nodes on the stack, add redundant nodes to the stack of reusable locations
void decrementRefs(int index) {
	
	//create search stack
	int stackSize = 10;
	int* combStack = (int*)malloc(stackSize * sizeof(int));
	int ptr = 0;
	HeapComb* currentNode;
	
	//start search from index
	*combStack = index;
	
	while (ptr >= 0) {
		
		//decrement ref count of current node
		currentNode = heap + *(combStack + ptr);
		currentNode->refs -=1;
		ptr--;

		if (currentNode->refs == 0) {
			//add redundant node to stack of free locations
			insertFreeNode(*(combStack + ptr + 1));
			
			//if a redundant node has children, decrement their reference counts too
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
}

//add a heap location to the stack of reusable addresses
void insertFreeNode(int index) {
	
	//allocate more memory to stack if necessary
	if (freeStackSize - 2 <= freeStackPtr) {
		freeStackSize *= 2;
		
		int* tempPtr = (int*)realloc(freeStack, freeStackSize * sizeof(int));
		if (!tempPtr) {
			reallocFail(freeStack);
		}
		
		freeStack = tempPtr;
		
	}
	
	//add location to top of stack
	freeStackPtr++;
	*(freeStack + freeStackPtr) = index;
}

//return a free heap address to write data to
int findMemory() {
	
	//if there aren't any reusable locations on the heap, the node will be added to the end of the heap
	if (freeStackPtr == -1) {
		checkReallocHeap();
		heapLength += 1;
		return heapLength - 1;
	} else {
		//overwrite redundant data on the heap
		freeStackPtr--;
		return *(freeStack + freeStackPtr + 1);
		
	}
}

//print heap in .sk format
void printHeapTree(int index) {
	HeapComb* i = heap + index;
	if (i->val == NULL) {
		printf("+");
		printHeapTree(i->left);
		printHeapTree(i->right);
	} else {
		printf("%s", i->val);
	}
}


void checkReallocSearchStack() {
	if (searchStackSize - 2 <= searchStackPtr) { 
		searchStackSize *= 2;
					    
		int* tempPtr = (int*)realloc(searchStack, searchStackSize * sizeof(int));
		if (!tempPtr) reallocFail(searchStack);
		searchStack = tempPtr;
	} 
}

// *************** reductions ***************

//perform reductions on a Comb term until none left
void runComb() {	
	
	bool reductionFound = true;
	int* indexPtr = malloc(sizeof(int));
	
	while (reductionFound) {
		
		//search for reduction from root
		reductionFound = false;
		char c = findReductionDFS(indexPtr);
		
		//if a possible reduction is found, perform it
		switch (c) {
			case 1:
				reductionFound = true;
				reduceK(*indexPtr);
				break;
			case 2:
				reductionFound = true;
				reduceS(*indexPtr);
				break;
			case 3:
				reductionFound = true;
				reduceI(*indexPtr);
				break;
		}
		
	}
}

//check if node is of the form ++Kab
bool matchK(int index) { 

	int i = (heap + index)->left;
	if (i != -1 && (heap + i)->left != -1) {
		char* str = (heap + (heap + i)->left)->val;
		return str != NULL && !strcmp(str, "K");
	}
	
	return false;
}

//check if node is of the form +++Sfgx
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

//check if node is of the form +Ix
bool matchI(int index) {
	int i = (heap + index)->left;
	return i != -1 && (heap + i)->val != NULL && !strcmp((heap + i)->val, "I");
}

//perform K- and S-reductions
void reduceK(int index) {
	
	//extract a from ++Kab
	HeapComb* i = heap + index;
	HeapComb* a = heap + (heap + i->left)->right;
	
	//increment the ref counts of a's children, since they'll be referenced by the new copy of a
	if (a->left != -1) { 
		(heap + a->left)->refs += 1;
		(heap + a->right)->refs += 1;
	}
	
	//decrement ref counts of +Ka and b
	decrementRefs(i->left);
	decrementRefs(i->right);
	
	//replace ++Kab with a
	editFrame(i, a->val, a->left, a->right, i->refs);

}

void reduceS(int index) {
	
	//extract locations of f, g, and x from +++Sfgx
	HeapComb* i = heap + index;
	int fIndex = (heap + (heap + i->left)->left)->right;
	int gIndex = (heap + i->left)->right;
	int xIndex = i->right;
	
	//increment ref counts of f, g, and x; decrement ref count of ++Sfg
	(heap + fIndex)->refs += 1;
	(heap + gIndex)->refs += 1;
	(heap + xIndex)->refs += 1;
	
	decrementRefs(i->left);
	
	//write +fx and +gx to heap
	int leftLocation = findMemory();
	editFrame(heap + leftLocation, NULL, fIndex, xIndex, 1);
	int rightLocation = findMemory();
	editFrame(heap + rightLocation, NULL, gIndex, xIndex, 1);
	
	//replace +++Sfgx with ++fx+gx
	editFrame(heap + index, NULL, leftLocation, rightLocation, i->refs);


}

void reduceI(int index) {
	//extract x from +Ix
	HeapComb* i = heap + index;
	HeapComb* x = heap + i->right;
	
	//increment the ref counts of x's children, since they'll be referenced by the new copy of x
	if (x->left != -1) {
		(heap + x->left)->refs += 1;
		(heap + x->right)->refs += 1;
	}
	
	//decrement ref counts of I and x
	decrementRefs(i->left);
	decrementRefs(i->right);
	
	//replace +Ix with x
	editFrame(i, x->val, x->left, x->right, i->refs);
	
}

//search for possible reductions starting from the root
int findReductionDFS(int* indexPtr) {
	
	//create search depth stack, which stores the ancestors of the node currently being searched
	//(makes it easier to control search depth)
	// int stackSize = 50;
	// int* combStack = (int*)malloc(stackSize * sizeof(int));
	// int ptr = 0;
	int result = 0;
	int index;
	int prevIndex;
	bool backtracking = false;
	
	searchStackPtr -=3;
	if (searchStackPtr < 0) searchStackPtr = 0;
	
	while (searchStackPtr != -1) {
		index = *(searchStack + searchStackPtr);
		
		if (!backtracking) {
			
			//look for reduction at ptr
			if (matchK(index)) {
				*indexPtr = index;
				//printf("found K\n"); fflush(stdout);
				//free(combStack);
				return 1;
			} else if (matchS(index)) {
				*indexPtr = index;
				//printf("found S\n"); fflush(stdout);
				//free(combStack);
				return 2;
			} else if (matchI(index)) {
				*indexPtr = index;
				//printf("found I\n"); fflush(stdout);
				//free(combStack);
				return 3;
			} else {
				int left = (heap + index)->left;
				
				//if node has left child, realloc if necessary and add left child to stack
				if (left != -1) {
					
					checkReallocSearchStack();
					
					searchStackPtr++;
					*(searchStack + searchStackPtr) = left;
				} else {
					//start backtracking to find a node in the stack that has a child and continue the search
					backtracking = true;
					searchStackPtr--;
					prevIndex = index;
				}
			}
		} else {
			
			//backtracking - if the prev node we checked is the left child of this one, add its right child to the stack
			if ((heap + index)->left == prevIndex) {
				
				checkReallocSearchStack();
				
				searchStackPtr++;
				*(searchStack + searchStackPtr) = (heap + index)->right;
				backtracking = false;
				
			} else {
				//this node has no unsearched children - keep backtracking
				searchStackPtr--;
				prevIndex = index;
			}
		}
	}
	
	//free(combStack);
	return result;
}



// *************** debugging ***************

//print stack of free heap locations
void printFreeStack() {
	printf("free memory stack:\n");
	for (int i = 0; i <= freeStackPtr; i++) {
		printf("%d: %d\n", i, *(freeStack + i)); fflush(stdout);
	}
	
}


void printSearchStack() {
	printf("searchPtr: %d\n", searchStackPtr);
	printf("search stack:\n");
	for (int i = 0; i <= searchStackPtr; i++) {
		printf("%d: %d\n", i, *(searchStack + i));
	}
}

//write most recent heap to file
void logHeap() {
	
	FILE* fptr = fopen("logFile.txt", "w");
	HeapComb* h = heap;
	int i;
	
	fprintf(fptr, "heapSize: %d\nheapLength: %d\n", heapSize, heapLength); fflush(fptr);

	for (i = 0; i < heapLength; i++) {

		if (h->val == NULL) {
			fprintf(fptr, "\n%d: (+, %d, %d, %d)", i, h->left, h->right, h->refs); fflush(fptr);
		} else {
			fprintf(fptr, "\n%d: (%s, %d, %d, %d)", i, h->val, h->left, h->right, h->refs); fflush(fptr);
		}
		
		h += 1;
	}
	
	fprintf(fptr, "\n"); fflush(fptr);
	fclose(fptr);
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
		
		h += 1;
	}
	printf("\n");
}

//exit program if no free memory available
void reallocFail(void* ptr) {
	printf("realloc failed. exiting program"); fflush(stdout);
	free(ptr);
	exit(1);
}
