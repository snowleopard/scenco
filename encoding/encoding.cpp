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
Encoding e;

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

	/***********************************************************************
	****************************BUILDING CPOG*******************************
	***********************************************************************/

	puts("\nOptimal scenarios encoding and CPOG synthesis.\n");	

	if(loadScenarios(file_in, fp) != 0){
		fprintf(stderr,"Loading scenarios failed.\n");
		return -1;
	}

	printf("\n%d scenarios have been loaded.\n", n);
	
	if(predicateSearch() != 0){
		fprintf(stderr,"Predicate searching failed.\n");
		return -1;
	}
	
	if( (fp = fopen(CONSTRAINTS_FILE,"w")) == NULL){
		fprintf(stderr,"Error on opening constraints file for writing.\n");
		return -1;
	}

	if(nonTrivialConstraints(fp, &total, &trivial) != 0){
		fprintf(stderr,"Non-trivial constraints searching failed.\n");
		return -1;
	}

	printf("\n%d non-trivial encoding constraints found:\n\n", total - trivial);

	if( (fp = fopen(TRIVIAL_ENCODING_FILE,"w")) == NULL){
		fprintf(stderr,"Error on opening constraints file for writing.\n");
		return -1;
	}
	for(int i = 0; i < total; i++)
		if (!encodings[i].trivial) {
			fprintf(fp,"%s\n",encodings[i].constraint.c_str());
			//printf("%s\n",encodings[i].constraint.c_str());
	}
	fclose(fp);
	
	printf("\nBuilding conflict graph... ");
	
	for(int i = 0; i < total; i++)
	if (!encodings[i].trivial)
	{
		string s = encodings[i].constraint;
		cgv.push_back(s);
		for(int j = 0; j < n; j++) if (s[j] == '0') s[j] = '1'; else if (s[j] == '1') s[j] = '0';
		cgv.push_back(s);
	}
	
	cge.resize(cgv.size());
	literal.resize(cgv.size());
	bestLiteral.resize(cgv.size());
	for(unsigned int i = 0; i < cgv.size(); i += 2) { bestLiteral[i] = i / 2; bestLiteral[i + 1] = -1;}
	
	for(unsigned int i = 0; i < cgv.size(); i++)
	for(unsigned int j = 0; j < cgv.size(); j++)
	{
		string a = cgv[i];
		string b = cgv[j];
		
		bool conflict = false;
		
		for(int k = 0; k < n; k++)
			if ((a[k] == '0' && b[k] == '1') || (a[k] == '1' && b[k] == '0'))
			{
				conflict = true;
				break;
			}
		
		if (conflict) cge[i].push_back(1); else cge[i].push_back(0);
	}
	
	printf("DONE.\n");
	fflush(stdout);

	switch(encoding){
		case single_literal:
			printf("Running single-literal encoding.\n");
			if(singleLiteralEncoding() != 0){
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


	return 0;
}
