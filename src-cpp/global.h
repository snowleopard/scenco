//TEMPORARY FILES
#if defined(__linux) || defined(__APPLE__)
	char TRIVIAL_ENCODING_FILE[] = "/tmp/trivial.XXXXXX";
	char CONSTRAINTS_FILE[] = "/tmp/constraints.XXXXXX";
	char TMP_FILE[] = "/tmp/tmpfile.XXXXXX";
	char SCRIPT_PATH[] = "/tmp/synth.XXXXXX";
#else
	char *TRIVIAL_ENCODING_FILE;
	char *CONSTRAINTS_FILE;
	char *TMP_FILE;
	char *SCRIPT_PATH;
#endif
	char LOG[] = "scenco.log";

// Alex's tool
FILE *fpLOG;
char *numb;
char **manual_file;
char **manual_file_back;
int *custom_perm;
int *custom_perm_back;
boolean *DC_custom = NULL;
int tot_enc;
boolean DC = FALSE;
boolean first = TRUE;
long long int num_perm, all_perm;
int **opt_diff = NULL;
int counter = 0;
int **perm = NULL;
char *file_cons = NULL;
long long int *weights = NULL;
boolean SET;
boolean unfix = FALSE;
//char **name_cond; 
//char **vertices;
char **diff = NULL; 
BitType **opcodes = NULL;
int bits;
int bits_saved;
int total;
int cpog_count = 0;
int *enc;
int *sol;
int len_sequence;
long long int minW;
long long int maxW;

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
