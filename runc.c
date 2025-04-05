#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <stdbool.h>
#include "runc.h"

/*
TODO:
 - check if memory management works right...
 - clean up
 it's slow :(
*/

bool logComb = false;

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
	runComb(&root, 1);
	printTree(root);
	
	//printf("\nref count: %d", root->right->refs);
	
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

bool runComb(Comb** comb, int headRefs) {
	if (logComb) { printf("\nsimplifying "); printTree(*comb); printf("\n"); }
	if ((*comb)->left == NULL) {
		//there is nothing we can do
		return false;
	} else if (matchK(*comb)) {
		reduceK(comb, headRefs);
		if (logComb) { printf("\ncheck - reduced to "); printTree(*comb); printf("\n"); }
		runComb(comb, headRefs); 
		return true;
	} else if (matchS((*comb))) {
		reduceS(*comb);	
		runComb(comb, headRefs);
		return true;
	} else {
		bool b1 = simplifyOneStep(&(*comb)->left, (*comb)->refs);
		if (!b1) {
			bool b2 = simplifyOneStep(&(*comb)->right, (*comb)->refs);
			if (!b2) return false;
			runComb(comb, headRefs);
			return true;
		} else {
			runComb(comb, headRefs);
			return true;
		}
	}

}

void reduceK(Comb** comb, int headRefs) {
	if (logComb) { printf("K-reducing "); printTree(*comb); printf("\n"); fflush(stdout); }
	Comb* ptr = *comb;
	Comb* a = ptr->left->right;
	//int i = ptr->refs;
	
	removeCombRef(ptr->left->left, headRefs, false);
	removeCombRef(ptr->left, headRefs, false);
	removeCombRef(ptr->right, headRefs, true);
	removeCombRef(ptr, headRefs, false);
	
	*comb = a;
	
	if (logComb) { printf("reduced to "); printTree(*comb); printf("\n"); fflush(stdout); }
}


void reduceS(Comb* comb) {
	if (logComb) { printf("S-reducing "); printTree(comb); printf("\n"); fflush(stdout); }
	
	int i = comb->refs;
	Comb* f = comb->left->left->right;
	Comb* g = comb->left->right;
	Comb* x = comb->right;
	
	removeCombRef(comb->left->left->left, i, false);
	removeCombRef(comb->left->left, i, false);
	removeCombRef(comb->left, i, false);
	
	addCombRef(x, i);
	
	Comb* fx = malloc(sizeof(Comb));
	fx->left = f;
	fx->right = x;
	fx->val = NULL;
	fx->refs = i;
	
	Comb* gx = malloc(sizeof(Comb));
	gx->left = g;
	gx->right = x;
	gx->val = NULL;
	gx->refs = i;
	
	
	comb->left = fx;
	comb->right = gx;
	
	if (logComb) { printf("reduced to "); printTree(comb); printf("\n"); fflush(stdout); }
	
}

bool simplifyOneStep(Comb** comb, int headRefs) {
	if (logComb) { printf("\nsimplifying "); printTree(*comb); printf("\n"); }
	if ((*comb)->left == NULL) {
		return false;
	} else if (matchK(*comb)) {	
		reduceK(comb, headRefs);	
		if (logComb) { printf("\ncheck - reduced to "); printTree(*comb); printf("\n"); }
		return true;
	} else if (matchS(*comb)) {
		reduceS(*comb);
		return true;
	} else {
		bool b1 = simplifyOneStep(&(*comb)->left, (*comb)->refs);
		if (!b1) {
			bool b2 = simplifyOneStep(&(*comb)->right, (*comb)->refs);
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

void addCombRef(Comb* comb, int i) {
	if (comb != NULL) {
		
		if (logComb) {
			printf("adding ref for "); printTree(comb);
			printf(" from %d to ", comb->refs);		
		}

		comb->refs += i;
		
		 if (logComb) printf("%d\n****\n", comb->refs);
		
		addCombRef(comb->left, i);
		addCombRef(comb->right, i);
	}
}

void removeCombRef(Comb* comb, int i, bool recurse) {
	if (comb != NULL) {
		if (logComb) {
			printf("removing ref for ");
			if (recurse) printTree(comb);
			printf(" from %d to ", comb->refs);
		}
		comb->refs -= i;
		
		if (logComb) printf("%d\n", comb->refs);
		
		if (recurse) {
			removeCombRef(comb->left, i, true);
			removeCombRef(comb->right, i, true);
		}
		
		if (comb->refs == 0) {
			if (logComb) printf("freeing node\n");
			freeCombNode(comb);
		}
		
		if (logComb) printf("\n*********\n");
	}
}

void freeCombNode(Comb* comb) {
	if (comb->val != NULL) {
		free(comb->val);
	}
	comb->val = NULL;
	comb->left = NULL;
	comb->right = NULL;
	comb->refs = 0;
	
	free(comb);
}
