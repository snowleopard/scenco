#ifdef __linux
	#include "encoding.h"
	#include "support_functions.cpp"
	#include "encoding_methods.cpp"
#else
	#include "[absolute_path]\encoding.h"
	#include "[absolute_path]\support_functions.cpp"
	#include "[absolute_path]\encoding_methods.cpp"
#endif

extern "C" int encoding_graphs(	char *file_in,
				char *custom_file_name,
				encodingType encoding){

	FILE *fp;
	int total;
	int trivial = 0;
	int bits;

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

	//**********************************************************************
	// Building CPOG Part
	//**********************************************************************

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

	//**********************************************************************
	// Preparation for encoding
	//**********************************************************************
	strcpy(file_in,TRIVIAL_ENCODING_FILE);
	file_cons = strdup(CONSTRAINTS_FILE);

	/*READ NON-TRIVIAL ENCODING FILE*/
	printf("Reading non-trivial encoding file... ");
	fflush(stdout);
	if( (err = read_file(file_in, &cpog_count, &len_sequence)) ){
		fprintf(stderr,"Error occured while reading non-trivial encoding file, error code: %d", err);
		removeTempFiles();
		return -1;
	}
	printf("DONE\n");
	fflush(stdout);

	/*SEED FOR RAND*/
	srand(time(NULL));

	/*ALLOCATING AND ZEROING DIFFERENCE MATRIX*/
	opt_diff = (int**) calloc(cpog_count, sizeof(int*));
	for(i=0;i<cpog_count;i++)
		opt_diff[i] = (int*) calloc(cpog_count, sizeof(int));

	/*NUMBER OF POSSIBLE ENCODING*/
	tot_enc = 1;
	for(i=0;i<bits;i++) tot_enc *= 2;

	/*ANALYSIS IF IT'S A PERMUTATION OR A DISPOSITION*/
	num_perm = 1;
	if (cpog_count == tot_enc){
		/*PERMUTATION*/
		if(!unfix && !SET){
			for(i = 1; i< tot_enc; i++)
				num_perm *= i;
		}else{
			for(i = 1; i<= tot_enc; i++)
				num_perm *= i;
		}
		printf("Number of possible permutations by fixing first element: %lld\n", num_perm);
	}
	else{
		/*DISPOSITION*/
		if(!unfix && !SET){
			elements = tot_enc-1;
			min_disp = elements - (cpog_count - 1) + 1;
		}else{
			elements = tot_enc;
			min_disp = elements - (cpog_count) + 1;
		}
			num_perm = 1;
		for(i=elements; i>= min_disp; i--)
			num_perm *= i;
		printf("Number of possible dispositions by fixing first element: %lld\n", num_perm);
	}

	if(encoding == exhaustive){
		if(num_perm > MAX_MEMORY || num_perm < 0){
			fprintf(stderr,"Solution space is too wide to be inspected.\n");
			removeTempFiles();
			return -1;
		}
	}else{
		num_perm = gen_perm;
	}

	/*PREPARATION DATA FOR ENCODING PERMUTATIONS*/
	enc = (int*) calloc(tot_enc, sizeof(int));

	/*First element is fixed*/
	if (!unfix && !SET)
		enc[0] = 1;
	
	sol = (int*) calloc(tot_enc, sizeof(int));
	if (sol == NULL){
		fprintf(stderr,"solution variable = null\n");
		removeTempFiles();
		return -1;
	}
	perm = (int**) malloc(sizeof(int*) * num_perm);
	if ( perm == NULL){
		fprintf(stderr,"perm variable = null\n");
		removeTempFiles();
		return -1;
	}
	for(i=0;i<num_perm;i++){
		perm[i] = (int*) malloc(cpog_count * sizeof(int));
		if (perm[i] == NULL){
			fprintf(stderr,"perm[%ld] = null\n",i);
			removeTempFiles();
			return 3;
		}
	}
	weights = (float*) calloc(num_perm, sizeof(float));

	/*BUILDING DIFFERENCE MATRIX*/
	printf("Building DM (=Difference Matrix)... ");
	fflush(stdout);
	if( (err = difference_matrix(cpog_count, len_sequence)) ){
		fprintf(stderr,"Error occurred while building difference matrix, error code: %d", err);
		removeTempFiles();
		return 3;
	}
	printf("DONE\n");
	fflush(stdout);

	//**********************************************************************
	// Reading encoding set by the user
	//**********************************************************************

	printf("Reading encodings set... ");
	fflush(stdout);
	if(read_set_encoding(custom_file_name,n,&bits) != 0){
		fprintf(stderr,"Error on reading encoding set.\n");
		removeTempFiles();
		return -1;
	}
	printf("DONE\n");

	printf("Check correcteness of encoding set... ");
	fflush(stdout);
	if(check_correctness(custom_file_name,n,tot_enc,bits) != 0){
		removeTempFiles();
		return -1;
	}
	printf("DONE\n");
	fflush(stdout);

	//**********************************************************************
	// Encoding part
	//**********************************************************************

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
				fprintf(stderr,"Sequential encoding failed.\n");
				return -1;
			}
			break;
		case satBased:
			printf("Running Sat-Based encoding.\n");
			if(satBasedEncoding() != 0){
				fprintf(stderr,"Sat-based encoding failed.\n");
				return -1;
			}
			break;
		case random_encoding:
			printf("Running Random encoding.\n");
			num_perm = 1;
			if(randomEncoding(n, tot_enc, bits) != 0){
				fprintf(stderr,"Random encoding failed.\n");
				return -1;
			}
			break;
		case heuristic:
			printf("Running Heuristic encoding.\n");
			if(heuristicEncoding() != 0){
				fprintf(stderr,"Heuristic encoding failed.\n");
				return -1;
			}
			break;
		case exhaustive:
			printf("Running Exhaustive encoding.\n");
			if(exhaustiveEncoding() != 0){
				fprintf(stderr,"Exhaustive encoding failed.\n");
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
	for(int i = 0; i < n; i++)
		printf("%s\n",scenarioOpcodes[i].c_str());

	return 0;
}
