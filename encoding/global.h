//TEMPORARY FILES
#ifdef __linux
	char TRIVIAL_ENCODING_FILE[FILENAME_LENGTH] = "/tmp/trivial.XXXXXX";
	char CONSTRAINTS_FILE[FILENAME_LENGTH] = "/tmp/constraints.XXXXXX";
	char TMP_FILE[FILENAME_LENGTH] = "/tmp/tmpfile.XXXXXX";
	char SCRIPT_PATH[FILENAME_LENGTH] = "/tmp/synth.XXXXXX";
#else
	char TRIVIAL_ENCODING_FILE[FILENAME_LENGTH];
	char CONSTRAINTS_FILE[FILENAME_LENGTH];
	char TMP_FILE[FILENAME_LENGTH];
	char SCRIPT_PATH[FILENAME_LENGTH];
#endif

// Alex's tool
char *numb;
char **manual_file;
char **manual_file_back;
int *custom_perm;
int *custom_perm_back;
boolean *DC_custom = NULL;
int tot_enc;
boolean DC = FALSE;
long long int num_perm;
int **opt_diff = NULL;
int counter = 0;
int **perm = NULL;
char *file_cons = NULL;
float *weights = NULL;
boolean SET;
boolean unfix = FALSE;
char **name_cond; 
char **vertices;
char **diff = NULL; 

// Andrey's tool
GRAPH_TYPE *g;
int n;
char s[stringLimit];

int V;
map<string, int> eventNames;
string eventNames_str[eventsLimit];
map<string, int> eventPredicates[eventsLimit];

vector<string> scenarioNames;
vector<string> scenarioOpcodes;

string ev[eventsLimit][predicatesLimit];
string ee[eventsLimit][eventsLimit];
map<string, vector<pair<int, int> > > constraints;
map<string, vector<pair<int, int> > >::iterator cp, cq;

vector<Encoding> encodings;

vector<string> cgv;
vector<vector<int> > cge;
vector<int> literal;
vector<int> bestLiteral;

string vConditions[eventsLimit][predicatesLimit];
string aConditions[eventsLimit][eventsLimit];

// alternative = false: alpha + beta * predicate
// alternative = true : alpha * (beta + predicate)
bool alternative = false;
