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
