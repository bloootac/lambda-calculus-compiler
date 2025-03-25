typedef struct Comb {
	char *val;
	struct Comb *left;
	struct Comb *right;
	
} Comb;

void strToTree(Comb* comb, Comb** left, Comb** right, char** val, char* str);
char* splitCombStr(char *str);
char* getVar(char *str);
void printTree(Comb *comb);
bool matchK(Comb* comb);
bool matchS(Comb* comb);
bool runComb(Comb** comb);
void reduceK(Comb** comb);
void reduceS(Comb** comb);
bool simplifyOneStep(Comb** comb);