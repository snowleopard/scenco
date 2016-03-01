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

	int get_n_graphs(){
		return cpog_count;
	}

	int load_graphs_codes(char *file_in,
			char *custom_file_name){

		FILE *fp;
		int trivial = 0;
		int err=0;

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
		file_cons = strdup(CONSTRAINTS_FILE);

		// reading non-trivial encoding file
		if( (err = read_file(TRIVIAL_ENCODING_FILE)) ){
			safe_exit("Error occured while reading non-trivial\
				   encoding file.");
			return -1;
		}

		// seed for rand
		srand(time(NULL));

		// difference matrix allocation
		opt_diff = (int**) calloc(n, sizeof(int*));
		for(int i=0;i<n;i++)
			opt_diff[i] = (int*) calloc(n, sizeof(int));

		if(encoding_memory_allocation() != 0){
			safe_exit("Error on allocating space for encoding\
				   \(graph loading phase).");
			return -1;
		}

		// building difference matrix
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

	int get_num_inputs(){		
		return nInputs;
	}

	char* get_input(int n){
		if(n >= nInputs){
			safe_exit("Wrong input ID.");
			return NULL;
		}
		return inputs[n];
	}

	void push_input(char* input){
		nInputs++;
		inputs = (char **) realloc(inputs, (nInputs) * sizeof(char*));
		if(inputs == NULL){
			safe_exit("Error on inputs allocation.");
			return;
		}
		inputs[nInputs-1] = strdup(input);
		return;
	}

	int get_num_outputs(){		
		return nOutputs;
	}

	char* get_output(int n){
		if(n >= nOutputs){
			safe_exit("Wrong output ID.");
			return NULL;
		}
		return outputs[n];
	}

	void push_output(char* output){
		nOutputs++;
		outputs = (char **) realloc(outputs, (nOutputs) * sizeof(char*));
		if(outputs == NULL){
			safe_exit("Error on outputs allocation.");
			return;
		}
		outputs[nOutputs-1] = strdup(output);
		return;
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

	void push_equation(char* equation){
		nEquations++;
		equations = (char **) realloc(equations, (nEquations) * sizeof(char*));
		if(equations == NULL){
			safe_exit("Error on equations allocation.");
			return;
		}
		equations[nEquations-1] = strdup(equation);
		return;
	}

	double get_area(){
		return area;
	}

	int get_gate_count(){
		return gates;
	}

	double map_and_get_area_circuit(char *abcPath, char *techLibrary){

		char *command;

		command = abcCommandOutTmp(abcPath);

		if(fill_up_mapping_file() != 0){
			safe_exit("Error writing equation into the tmp file.");
			return -1;
		}

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

		if(fill_up_mapping_file() != 0){
			safe_exit("Error writing equation into the tmp file.");
			return -1;
		}

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

		if(inputs != NULL){
			for(int i = 0; i<nInputs;i++){
				if(inputs[i] != NULL) free(inputs[i]);
			}
			free(inputs);
			inputs = NULL;
			nInputs = 0;
		}

		if(outputs != NULL){
			for(int i = 0; i<nOutputs;i++){
				if(outputs[i] != NULL) free(outputs[i]);
			}
			free(outputs);
			outputs = NULL;
			nOutputs = 0;
		}

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
