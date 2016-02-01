#ifdef __linux
	#include "encoding.h"
	#include "support_functions.cpp"
	#include "encoding_methods.cpp"
#else
	#include "[absolute_path]\encoding.h"
	#include "[absolute_path]\support_functions.cpp"
	#include "[absolute_path]\encoding_methods.cpp"
#endif

extern "C" int encoding_graphs(char *file_in, encodingType encoding){

	FILE *fp;
	int total, trivial = 0;

	// memory allocation
	printf("Allocating memory for vertex names and graphs...");
	fflush(stdout);
	name_cond = (char**) malloc(sizeof(char*) * MAX_VERT);
	vertices = (char**) malloc(sizeof(char*) * MAX_VERT);
	g = (GRAPH_TYPE *) malloc(sizeof(GRAPH_TYPE) * scenariosLimit);
	printf("DONE\n");
	fflush(stdout);

	if(temporary_files_creation() != 0){
		return -1;
	}

	// loading scenarios
	puts("\nOptimal scenarios encoding and CPOG synthesis.\n");	
	if(loadScenarios(file_in, fp) != 0){
		fprintf(stderr,"Loading scenarios failed.\n");
		return -1;
	}
	printf("\n%d scenarios have been loaded.\n", n);
	
	// looking for predicates
	if(predicateSearch() != 0){
		fprintf(stderr,"Predicate searching failed.\n");
		return -1;
	}
	
	// looking for non-trivial constraints
	if( (fp = fopen(CONSTRAINTS_FILE,"w")) == NULL){
		fprintf(stderr,"Error on opening constraints file for writing.\n");
		return -1;
	}
	if(nonTrivialConstraints(fp, &total, &trivial) != 0){
		fprintf(stderr,"Non-trivial constraints searching failed.\n");
		return -1;
	}
	printf("\n%d non-trivial encoding constraints found:\n\n", total - trivial);

	// writing non-trivial constraints into a file
	if( (fp = fopen(TRIVIAL_ENCODING_FILE,"w")) == NULL){
		fprintf(stderr,"Error on opening constraints file for writing.\n");
		return -1;
	}
	for(int i = 0; i < total; i++)
		if (!encodings[i].trivial) {
			fprintf(fp,"%s\n",encodings[i].constraint.c_str());
	}
	fclose(fp);
	
	printf("\nBuilding conflict graph... ");
	if(conflictGraph(&total) != 0){
		fprintf(stderr,"Building conflict graph failed.\n");
		return -1;
	}
	printf("DONE.\n");
	fflush(stdout);

	switch(encoding){
		case single_literal:
			printf("Running single-literal encoding.\n");
			if(singleLiteralEncoding(total) != 0){
				fprintf(stderr,"Single-literal encoding failed.\n");
				return -1;
			}
			break;
		case sequential:
			printf("Running Sequential encoding.\n");
			if(sequentialEncoding() != 0){
				fprintf(stderr,"Single-literal encoding failed.\n");
				return -1;
			}
			break;
		case satBased:
			printf("Running Sat-Based encoding.\n");
			if(satBasedEncoding() != 0){
				fprintf(stderr,"Single-literal encoding failed.\n");
				return -1;
			}
			break;
		case random_encoding:
			printf("Running Random encoding.\n");
			if(randomEncoding() != 0){
				fprintf(stderr,"Single-literal encoding failed.\n");
				return -1;
			}
			break;
		case heuristic:
			printf("Running Heuristic encoding.\n");
			if(heuristicEncoding() != 0){
				fprintf(stderr,"Single-literal encoding failed.\n");
				return -1;
			}
			break;
		case exhaustive:
			printf("Running Exhaustive encoding.\n");
			if(exhaustiveEncoding() != 0){
				fprintf(stderr,"Single-literal encoding failed.\n");
				return -1;
			}
			break;
		default:
			fprintf(stderr, "Undefined encoding method selected.\n");
			return -1;
	}
	printf("DONE.\n");
	fflush(stdout);

	printf("\nOpcodes assigned to the graphs:\n");
	for(int i = 0; i < n; i++) printf("%s\n",scenarioOpcodes[i].c_str());

	return 0;
}
