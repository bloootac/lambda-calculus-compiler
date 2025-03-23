// typedef struct {
	// char *head;
	// char *tail;
// } SplitComb;

typedef struct Comb {
	char *val;
	struct Comb *left;
	struct Comb *right;
	
} Comb;

void strToTree(Comb* comb, Comb** left, Comb** right, char** val, char* str);
char* splitCombStr(char *str);
char* getVar(char *str);
void printTree(Comb *comb);
