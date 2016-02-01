// libraries import
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>
//#include <sys/stat.h>
//#include <sys/time.h>
//#include <ctime>
//#include <cctype>
//#include <unistd.h>
//#include <algorithm>
//#include <iostream>
//#include <fstream>
//#include <math.h>
#include <sstream>
#include <map>
//#include <set>

// encodings available
enum encodingType
{ 
	single_literal,
	sequential,
	satBased,
	random_encoding,
	heuristic,
	exhaustive
}; 

using namespace std;

// constant definition
#define MAX_VERT	10000	//Max number of vertices of CPOG
#define FILENAME_LENGTH	300	// Max length of file names

#define stringLimit	1000
#define eventsLimit	1000
#define scenariosLimit	700
#define predicatesLimit	200

// global variables
char **name_cond; 
char **vertices;

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

// type definition
typedef enum {FALSE, TRUE} boolean;
typedef struct struct_tag{
	char type; 		/*v = vertex, e = edge*/
	char* source, *dest; 	/*if n
				  	source = vertex_name
			       	  if e
				  	source = from vertex
				  	dest = to vertex*/
	boolean condition; 	/*if TRUE, a condition exists*/
	char* cond; 		/*condition on vertex*/
	char* truth, *truth_cond;/*truth table*/
	char **fun, **fun_cond; /*boolean function*/
}CPOG_TYPE;

typedef struct Graph_st
{
	int e[eventsLimit][eventsLimit];
	int v[eventsLimit];
	int pred[eventsLimit];
	
	bool transitiveClosure()
	{
		for(int i = 0; i < eventsLimit; i++)
		if (v[i])
			for(int j = 0; j < eventsLimit; j++)
			if (v[j] && e[j][i])
				for(int k = 0; k < eventsLimit; k++)
				if (v[k] && e[i][k]) e[j][k] = 1;
		
		for(int i = 0; i < eventsLimit; i++) if (e[i][i]) return false;
		
		for(int i = 0; i < eventsLimit; i++)
		if (v[i])
			for(int j = 0; j < eventsLimit; j++)
			if (v[j] && e[j][i])
				for(int k = 0; k < eventsLimit; k++)
				if (v[k] && e[i][k]) e[j][k] = 2;

		return true;
	}
	
}GRAPH_TYPE;

typedef struct Encoding_st
{
	string constraint;
	
	bool trivial;
	int constant;
	
	int literal;
	bool inverted;
}Encoding;

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
