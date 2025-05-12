typedef struct Comb {
	char* val;
	struct Comb* left;
	struct Comb* right;
} Comb;

typedef struct HeapComb {
	char* val;
	int left;
	int right;
	int refs;
} HeapComb;

void runFile(char* input);
void readFile(FILE* fptr, char* input);

void strToTree(Comb** left, Comb** right, char** val, char* str);
char* splitCombStr(char *str);
char* getVar(char *str);
void printTree(Comb *comb);
void freeCombTree(Comb* ptr);

void initHeap();
void buildHeap(Comb* comb, int index);
void printHeap();
void checkReallocHeap();
void editFrame(HeapComb* i, char* val, int left, int right, int refs);
void decrementRefs(int index);
void insertFreeNode(int index);
int findMemory();

void initSearchStack();
void checkReallocSearchStack();

void runComb();
bool matchK(int index);
bool matchS(int index);
bool matchI(int index);
void reduceK(int index);
void reduceS(int index);
void reduceI(int index);
int findReductionDFS(int* indexPtr);
void printHeapTree(int index);

void reallocFail(void* ptr);
void printFreeNodes();
void printSearchStack();
void logHeap();