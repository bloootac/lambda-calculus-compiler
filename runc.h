typedef struct Comb {
	char* val;
	struct Comb* left;
	struct Comb* right;
	int refs;
} Comb;

void strToTree(Comb** left, Comb** right, char** val, int* refs, char* str);
char* splitCombStr(char *str);
char* getVar(char *str);
void printTree(Comb *comb);
bool matchK(Comb* comb);
bool matchS(Comb* comb);
bool runComb(Comb** comb, int headRefs);
void reduceK(Comb** comb, int headRefs);
void reduceS(Comb* comb);
bool simplifyOneStep(Comb** comb, int headRefs);
void freeComb(Comb* comb);
Comb* copyComb(Comb* comb);
void addCombRef(Comb* comb, int i);
void removeCombRef(Comb* comb, int i, bool recurse);
void freeCombNode(Comb* comb);