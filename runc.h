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


void runComb();
void reduceK(int index);
void reduceS(int index);
char findReduction(int* indexPtr);
char findReductionHelper(int* indexPtr, int index, int depth, bool* notFinished);

void initHeap();
void buildHeap(Comb* comb, int index);
void printHeap();
void checkReallocHeap();
void editFrame(HeapComb* i, char* val, int left, int right);
void heapToTree(int index);
void logHeap();