#include "encoding.h"
#include "support_functions.cpp"

extern "C" int encoding_graphs(char *file_in){

	FILE *fp;

	printf("Allocating memory for vertex names and graphs...");
	fflush(stdout);
	name_cond = (char**) malloc(sizeof(char*) * MAX_VERT);
	vertices = (char**) malloc(sizeof(char*) * MAX_VERT);
	g = (GRAPH_TYPE *) malloc(sizeof(GRAPH_TYPE) * scenariosLimit);
	printf("DONE\n");
	fflush(stdout);

	/*************************************************************************
	****************************BUILDING CPOG*********************************
	*************************************************************************/

	puts("\nOptimal scenarios encoding and CPOG synthesis.\n");	

	if (!alternative)
		puts("Using 'f = x + y * predicate' to deal with predicates.\n");
	else
		puts("Using 'f = x * (y + predicate)' to deal with predicates.\n");

	fp = freopen(file_in, "r", stdin);
	if (fp == NULL){
		fprintf(stderr, "Error opening %s\n", file_in);
		return -1;
	}

	n = 0;
	while(scanf("%s", s) == 1)
	{
		if (s[0] == '#')
		{
			if(fgets(s, sizeof(s), stdin) == NULL){
				fprintf(stderr,"Error reading scenario.\n");
				fclose(fp);
				return -1;
			}
			continue;
		}
		
		if (!strcmp(s, ".scenario"))
		{
			if(scanf("%s", s) == EOF){
				fprintf(stderr,"Error reading scenario.\n");
				fclose(fp);
				return -1;
			}
			printf("Loading scenario '%s'... ", s);
			scenarioNames.push_back(s);
			if (!readScenario()) {
				fprintf(stderr,"Error reading scenario.\n");
				fclose(fp);
				return -1;
			}
		}
		else
		{
			fprintf(stderr,"Wrong file format.\n");
			fclose(fp);
			return -1;
		}
	}

	
	printf("\n%d scenarios have been loaded.\n", n);
	
	return 0;
}
