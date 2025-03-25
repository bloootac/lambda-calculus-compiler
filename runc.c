#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <stdbool.h>
#include "runc.h"

/*
TODO:
 - fix runComb bug [done] (i don't understand why but it's gone)
 - figure out when to free memory locations? it stops working when i try
 - add simplifyOneStep
 - add kPrune ? 
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
	strToTree(root, &(root->left), &(root->right), &(root->val), input);
	printTree(root);
	
	fflush(stdout);
	
	printf("\nrun:\n");
	runComb(&root);
	printTree(root);
	
    //close file, de-allocate memory
    fclose(fptr);
    free(input);
    
    exit(0); 
}

void strToTree(Comb* comb, Comb** left, Comb** right, char** val, char* str) {
	
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
		strToTree(*left, &((*left)->left), &((*left)->right), &((*left)->val), str + 1);
		//right = B
		strToTree(*right, &((*right)->left), &((*right)->right), &((*right)->val), splitIndex);
		
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
		
		if (comb->left == NULL)  {
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

/*
run_c :: Comb -> Comb

run_c (App (App K a) b) = run_c $ k_prune prune_lim a
run_c (App (App (App K a) b) c) = run_c $ k_prune prune_lim (App a c)
run_c (App c (App (App K a) b)) = run_c $ k_prune prune_lim(App c a)
run_c (App (App (App S f) g) x) = run_c $ k_prune prune_lim (App (App f x) (App g x))

run_c (App x y) = let x' = simplify_one_step x
                  in if (x == x') then 
                      let y' = simplify_one_step y 
                      in if (y == y') then k_prune prune_lim (App x y) else run_c $ k_prune prune_lim (App x y')
                  else run_c $ k_prune prune_lim (App x' y)
                     
run_c x = x
   
simplify_one_step :: Comb -> Comb
simplify_one_step (App (App K a) b) = a
simplify_one_step (App (App (App S f) g) x) = (App (App f x) (App g x))
simplify_one_step (App x y) = let x' = simplify_one_step x
                              in if (x == x') then 
                                  let y' = simplify_one_step y 
                                  in if (y == y') then (App x y) else (App x y')
                              else (App x' y)
simplify_one_step x = x
*/

void reduceK(Comb** comb) {
	*comb = (*comb)->left->right;
}

void reduceS(Comb** comb) {
	Comb* f = (*comb)->left->left->right;
	Comb* g = (*comb)->left->right;
	Comb* x = (*comb)->right;
	
	(*comb)->left = malloc(sizeof(Comb));
	(*comb)->left->val = NULL;
	(*comb)->left->left = f;
	(*comb)->left->right = x;
	
	(*comb)->right = malloc(sizeof(Comb));
	(*comb)->right->val = NULL;
	(*comb)->right->left = g;
	(*comb)->right->right = x;
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
