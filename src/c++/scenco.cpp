#include <ctime>
#include "base.h"
#include "utilities.cpp"
#include "load_scenarios.cpp"
#include "heuristic_function.cpp"
#include "custom_encoding.cpp"
#include "encoding_methods.cpp"
#include "mapping.cpp"
#include "bool_minimisation.cpp"

extern "C" {

	int get_bit(int row, int col){
		return ((int) opcodes[row][col]);
	}

	int get_codes_length(){
		return bits;
	}

	int load_graphs_codes(char *file_in,
			char *custom_file_name){

		FILE *fp;
		int trivial = 0;
		int err=0;
		int elements;
		int min_disp;

		if(first){
			first = FALSE;
			if(temporary_files_creation() != 0){
				safe_exit("Error allocating temporary files.");
				return -1;
			}
		}

		// memory allocation
		if(heapVariablesAllocation() != 0){
			safe_exit("Error on heap allocation variables.");
			return -1;
		}

		/***************************************************************
		*                     Building CPOG Part                       *
		***************************************************************/

		// loading scenarios
		if(loadScenarios(file_in, fp) != 0){
			safe_exit("Loading scenarios failed.");
			return -1;
		}

		// looking for predicates
		if(predicateSearch() != 0){
			safe_exit("Predicate searching failed.");
			return -1;
		}

		// looking for non-trivial constraints
		if( (fp = fopen(CONSTRAINTS_FILE,"w")) == NULL){
			safe_exit("Error on opening constraints file for writing.");
			return -1;
		}
		if(nonTrivialConstraints(fp, &total, &trivial) != 0){
			safe_exit("Non-trivial constraints searching failed.");
			return -1;
		}
		fclose(fp);

		// writing non-trivial constraints into a file
		if( (fp = fopen(TRIVIAL_ENCODING_FILE,"w")) == NULL){
			safe_exit("Error on opening constraints file for writing.");
			return -1;
		}
		for(int i = 0; i < total; i++)
			if (!encodings[i].trivial) {
				fprintf(fp,"%s\n",encodings[i].constraint.c_str());
		}
		fclose(fp);

		if(conflictGraph(&total) != 0){
			safe_exit("Building conflict graph failed.");
			return -1;
		}

		/***************************************************************
		*              Reading encoding set by the user                *
		***************************************************************/

		if(read_set_encoding(custom_file_name,n,&bits) != 0){
			safe_exit("Error on reading encoding set.");
			return -1;
		}

		if(check_correctness(custom_file_name,n,tot_enc,bits) != 0){
			safe_exit("Codes set by the user unfeasible.");
			return -1;
		}

		/***************************************************************
		*               Variable preparation for encoding              *
		***************************************************************/
		strcpy(file_in,TRIVIAL_ENCODING_FILE);
		file_cons = strdup(CONSTRAINTS_FILE);

		/*READ NON-TRIVIAL ENCODING FILE*/
		if( (err = read_file(file_in)) ){
			safe_exit("Error occured while reading non-trivial\
				   encoding file.");
			return -1;
		}

		/*SEED FOR RAND*/
		srand(time(NULL));

		/*ALLOCATING AND ZEROING DIFFERENCE MATRIX*/
		opt_diff = (int**) calloc(n, sizeof(int*));
		for(int i=0;i<n;i++)
			opt_diff[i] = (int*) calloc(n, sizeof(int));

		/*NUMBER OF POSSIBLE ENCODING*/
		tot_enc = 1;
		for(int i=0;i<bits;i++) tot_enc *= 2;

		/*ANALYSIS IF IT'S A PERMUTATION OR A DISPOSITION*/
		num_perm = 1;
		if (n == tot_enc){
			/*PERMUTATION*/
			if(!unfix && !SET){
				for(int i = 1; i< tot_enc; i++)
					num_perm *= i;
			}else{
				for(int i = 1; i<= tot_enc; i++)
					num_perm *= i;
			}
		}
		else{
			/*DISPOSITION*/
			if(!unfix && !SET){
				elements = tot_enc-1;
				min_disp = elements - (n- 1) + 1;
			}else{
				elements = tot_enc;
				min_disp = elements - (n) + 1;
			}
				num_perm = 1;
			for(int i=elements; i>= min_disp; i--)
				num_perm *= i;
		}

		all_perm = num_perm;

		/*PREPARATION DATA FOR ENCODING PERMUTATIONS*/
		enc = (int*) calloc(tot_enc, sizeof(int));

		/*First element is fixed*/
		if (!unfix && !SET)
			enc[0] = 1;

		sol = (int*) calloc(tot_enc, sizeof(int));
		if (sol == NULL){
			safe_exit("Error on allocating array for the solution.");
			return -1;
		}

		/*BUILDING DIFFERENCE MATRIX*/
		if( (err = difference_matrix(cpog_count)) ){
			safe_exit("Error occurred while building difference\
				   matrix");
			return -1;
		}

		return 0;
	}

	int single_literal_encoding(){

		computeCodesAvailable();

		if(singleLiteralEncoding(total) != 0){
			safe_exit("Single-literal encoding failed.");
			return -1;
		}

		if (export_variables(single_literal) != 0){
			safe_exit("Error exporting variables for external environment\
				   for single-literal encoding.");
			return -1;
		}

		synthesisSpaceSingleLiteral();

		return 0;
	}

	int sequential_encoding(){

		computeCodesAvailable();

		if(allocate_encodings_space(1) != 0){
			safe_exit("Error allocating space for sequential encoding.");
			return -1;
		}

		if(sequentialEncoding() != 0){
			safe_exit("Sequential encoding failed.");
			return -1;
		}

		if (export_variables(sequential) != 0){
			safe_exit("Error exporting variables for external environment\
				   for sequential encoding.");
			return -1;
		}

		opcodesForSynthesis(0);

		return 0;
	}

	int random_encoding(int num_enc){

		bits = bits_saved;
		computeCodesAvailable();

		if(allocate_encodings_space(num_enc) != 0){
			safe_exit("Encoding allocating variables for random encoding.");
			return -1;
		}

		if(randomEncoding() != 0){
			safe_exit("Random encoding failed.");
			return -1;
		}

		if(heuristic_choice() != 0) {
			safe_exit("Error on choosing best results on random encoding.");
			return -1;
		}

		if (export_variables(randomE) != 0){
			safe_exit("Error exporting variables for external environment\
				   for random encoding.");
			return -1;
		}

		return 0;
	}

	int heuristic_encoding(int num_enc){

		bits = bits_saved;
		computeCodesAvailable();

		if(allocate_encodings_space(num_enc) != 0){
			safe_exit("Encoding allocating variables for heuristic encoding.");
			return -1;
		}
		if(randomEncoding() != 0){
			safe_exit("Random encoding failed (heuristic encoding).");
			return -1;
		}
		if(start_simulated_annealing() != 0){
			safe_exit("Heuristic optimisation failed.");
			return -1;
		}

		if(heuristic_choice() != 0) {
			safe_exit("Error on choosing best results on heuristic encoding.");
			return -1;
		}

		if (export_variables(heuristic) != 0){
			safe_exit("Error exporting variables for external environment\
				   for heuristic encoding.");
			return -1;
		}

		return 0;
	}

	int exhaustive_encoding(int num_enc){

		if(allocate_encodings_space(num_enc) != 0){
			safe_exit("Encoding allocating variables for exhaustive encoding.");
			return -1;
		}

		computeCodesAvailable();

		if(!unfix && !SET){
			//permutation_stdalgo(cpog_count,tot_enc);
			exhaustiveEncoding(sol,0,enc,cpog_count, tot_enc);
		}else{
			exhaustiveEncoding(sol,-1,enc,cpog_count, tot_enc);
			filter_encodings(cpog_count, bits, tot_enc);

		}

		if(heuristic_choice() != 0) {
			safe_exit("Error on choosing best results on exhaustive encoding.");
			return -1;
		}

		if (export_variables(exhaustive) != 0){
			safe_exit("Error exporting variables for external environment\
				   for exhaustive encoding.");
			return -1;
		}

		return 0;
	}

	int get_controller_formulae(char* abcPath){

		int err;

		if (get_formulae_nodes(abcPath) != 0){
			safe_exit("Error using ABC for getting Boolean formulae\
				   of nodes and arcs (controller synthesis).");
			return -1;
		}

		if( (err = equations_abc(cpog_count,bits)) != 0){
			safe_exit("Error merging final equations for ABC mapping\
				   (controller synthesis).");
			return -1;
		}

		freeBooleanFunctions();

		return 0;
	}

	int get_CPOG_formulae(char* abcPath){

		int err;

		if (get_formulae_nodes(abcPath) != 0){
			removeTempFiles();
			safe_exit("Error using ABC for getting Boolean formulae\
				   of nodes and arcs (CPOG synthesis).");
			return -1;
		}

		if( (err = equations_abc_cpog_size(cpog_count,bits)) != 0){
			safe_exit("Error merging final equations for ABC mapping\
				   (CPOG synthesis).");
			return -1;
		}

		freeBooleanFunctions();

		return 0;
	}

	int get_num_equations(){		
		return nEquations;
	}

	char* get_equation(int n){
		if(n >= nEquations){
			safe_exit("Wrong equationg ID.");
			return NULL;
		}
		return equations[n];
	}

	double map_and_get_area_circuit(char *abcPath, char *techLibrary){

		char *command;

		command = abcCommandOutTmp(abcPath);

		if(writeMappingScript(techLibrary) != 0){
			safe_exit("Error writing mapping script for ABC.");
			return -1;
		}

		// run abc
		if( system(command) == -1){
			safe_exit("Error running ABC for mapping getting area of\
				   the circuit.");
			return -1;
		}

		if(parse_area_result_abc() != 0){
			safe_exit("Error parsing ABC result for getting Area\
				   of the circuit.");
			return -1;
		}

		free(command);

		return area;
	}

	int generate_verilog(char *abcPath, char *techLibrary, char *vFile){

		char *command;

		command = abcCommandOutNull(abcPath);

		if(writeVerilogGenScript(techLibrary, vFile) != 0){
			safe_exit("Error writing verilog script for ABC.");
			return -1;
		}

		// run abc
		if( system(command) == -1){
			safe_exit("Error running Abc for verilog generation.");
			return -1;
		}

		free(command);

		return 0;
	}

	void free_formulae(){

		if(equations != NULL){
			for(int i = 0; i<nEquations;i++){
				if(equations[i] != NULL) free(equations[i]);
			}
			free(equations);
			equations = NULL;
			nEquations = 0;
		}
		area = 0;
		gates = 0;
		return;
	}

	int unload_graphs_codes(){

		freeVariables();

		resetVariables();

		removeTempFiles();
		return 0;
	}
}
