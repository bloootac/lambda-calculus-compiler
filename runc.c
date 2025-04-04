#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <stdbool.h>
#include "runc.h"

/*
TODO:
 - add reference count to Comb type
*/

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
	
	printf("\nrun:\n");
	runComb(&root);
	printTree(root);
	
	printf("\nref count: %d", root->right->refs);
	
    //close file, de-allocate memory. TODO de-allocate root too
    fclose(fptr);
    free(input);
    
    exit(0); 
}

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

bool matchK(Comb* comb) {
	return (comb != NULL && comb->left != NULL && comb->left->left != NULL && comb->left->left->val != NULL && !strcmp(comb->left->left->val, "K"));
}

bool matchS(Comb* comb) {
	return (comb != NULL && comb->left != NULL && comb->left->left != NULL && comb->left->left->left != NULL && comb->left->left->left->val != NULL && !strcmp(comb->left->left->left->val, "S"));
}

bool runComb(Comb** comb) {
	
	//TODO: we get a seg fault when running fib 5 f x...
	//maybe related to issue w/ freeing memory? runComb being run on a free ptr when it shouldn't be?
	if ((*comb)->left == NULL) {
		//there is nothing we can do
		return false;
	} else if (matchK(*comb)) {
		reduceK(comb);
		runComb(comb);
		return true;
	} else if (matchS(*comb)) {
		reduceS(comb);	
		runComb(comb);
		return true;
	} else {
		bool b1 = simplifyOneStep(&(*comb)->left);
		if (!b1) {
			bool b2 = simplifyOneStep(&(*comb)->right);
			if (!b2) return false;
			runComb(comb);
			return true;
		} else {
			runComb(comb);
			return true;
		}
	}

}

void reduceK(Comb** comb) {
	Comb* a = (*comb)->left->right;
	
	printf("K-reducing "); printTree(*comb); printf("\n"); fflush(stdout);
	
	removeCombRef((*comb)->left->left, false);
	removeCombRef((*comb)->left, false);
	removeCombRef((*comb)->right, true);
	removeCombRef((*comb), false); //TODO: check ...
	
	
	*comb = a;
	
	printf("reduced to "); printTree(*comb); printf("\n"); fflush(stdout);
}

void reduceS(Comb** comb) {
	
	printf("S-reducing "); printTree(*comb); printf("\n"); fflush(stdout);
	
	Comb* f = (*comb)->left->left->right;
	Comb* g = (*comb)->left->right;
	Comb* x = (*comb)->right;
	
	removeCombRef((*comb)->left->left->left, false);
	removeCombRef((*comb)->left->left, false);
	removeCombRef((*comb)->left, false);
	removeCombRef(*comb, false);
	
	addCombRef(x);
	
	*comb = malloc(sizeof(Comb));
	(*comb)->refs = 1;
	(*comb)->val = NULL;
	
	
	(*comb)->left = malloc(sizeof(Comb));
	(*comb)->left->refs = 1;
	(*comb)->left->val = NULL;
	(*comb)->left->left = f;
	(*comb)->left->right = x;
	
	(*comb)->right = malloc(sizeof(Comb));
	(*comb)->right->refs = 1;
	(*comb)->right->val = NULL;
	(*comb)->right->left = g;
	(*comb)->right->right = x;
	
	printf("reduced to "); printTree(*comb); printf("\n"); fflush(stdout);
}

bool simplifyOneStep(Comb** comb) {
	if ((*comb)->left == NULL) {
		return false;
	} else if (matchK(*comb)) {	
		reduceK(comb);	
		return true;
	} else if (matchS(*comb)) {
		reduceS(comb);
		return true;
	} else {
		bool b1 = simplifyOneStep(&(*comb)->left);
		if (!b1) {
			bool b2 = simplifyOneStep(&(*comb)->right);
			return b2;
		}
		return true;
	}
}

void freeComb(Comb* comb) {
	if (comb != NULL) {
		if (comb->left == NULL) {
			free(comb->val);
			free(comb);
		}	
		else { 
			freeComb(comb->left);
			freeComb(comb->right);
			free(comb);
		}
	}
}

//we don't need this...
Comb* copyComb(Comb* comb) {
	Comb* newComb = malloc(sizeof(Comb));
	newComb->refs = comb->refs;
	if (comb->left == NULL) {
		newComb->val = malloc(1 + strlen(comb->val));
		strcpy(newComb->val, comb->val);
		newComb->left = NULL;
		newComb->right = NULL;
	} else {
		newComb->val = NULL;
		newComb->left = copyComb(comb->left);
		newComb->right = copyComb(comb->right);
	}
	return newComb;
}

void addCombRef(Comb* comb) {
	if (comb != NULL) {
		
		printf("adding ref for "); printTree(comb);
		printf(" from %d to ", comb->refs);
		
		comb->refs += 1;
		
		printf("%d\n****\n", comb->refs);
		
		addCombRef(comb->left);
		addCombRef(comb->right);
	}
}

void removeCombRef(Comb* comb, bool recurse) {
	if (comb != NULL) {
		
		printf("removing ref for ");
		if (recurse) printTree(comb);
		printf(" from %d to ", comb->refs);
		
		comb->refs -=1;
		
		printf("%d\n", comb->refs);
		
		if (recurse) {
			removeCombRef(comb->left, true);
			removeCombRef(comb->right, true);
		}
		
		if (comb->refs == 0) {
			printf("freeing node\n");
			freeCombNode(comb);
		}
		
		printf("\n*********\n");
	}
}

void freeCombNode(Comb* comb) {
	//if var, free var, refs, then ptr
	//else, free left, right, refs, then ptr
	if (comb->val != NULL) {
		free(comb->val);
		//do i need to free an int?
	}
	comb->val = NULL;
	comb->left = NULL;
	comb->right = NULL;
	comb->refs = 0;
	//*comb = NULL;
	
	free(comb);
}