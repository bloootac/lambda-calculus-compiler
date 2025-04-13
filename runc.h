typedef struct Comb {
	char* val;
	struct Comb* left;
	struct Comb* right;
	int refs;
} Comb;

typedef struct HeapComb {
	char* val;
	int left;
	int right;
} HeapComb;

void strToTree(Comb** left, Comb** right, char** val, int* refs, char* str);
char* splitCombStr(char *str);
char* getVar(char *str);
void printTree(Comb *comb);

bool matchK(int index);
bool matchS(int index);


bool runComb(int index);
void reduceK(int index);
void reduceS(int index);
bool simplifyOneStep(int index);

void freeComb(Comb* comb);
Comb* copyComb(Comb* comb);
void addCombRef(Comb* comb, int i);
void removeCombRef(Comb* comb, int i, bool recurse);
void freeCombNode(Comb* comb);

void initHeap();
void buildHeap(Comb* comb, int index);
void printHeap();
void checkReallocHeap();
void editFrame(HeapComb* i, char* val, int left, int right);