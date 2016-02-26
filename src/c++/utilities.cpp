/*It concatenates two strings creating the right portion in the memory.*/
char* catMem(char *str1, char *str2){

	char *newStr;

	newStr = (char *) malloc(sizeof(char) * (strlen(str1) + strlen(str2) + 1));
	sprintf(newStr, "%s%s", str1, str2);
	
	free(str1);

	return newStr;
}

/*It concatenates one string with a character creating the right portion in 
the memory.*/
char* catChar(char *str1, char c){

	char *newStr;

	newStr = (char *) malloc(sizeof(char) * (strlen(str1) + 2));
	sprintf(newStr, "%s%c", str1, c);
	
	free(str1);

	return newStr;
}

// Check file existance
int FileExists(char *filename)
{
	FILE *fp = fopen (filename, "r");
	if (fp!=NULL) fclose (fp);
	return (fp!=NULL);
}

// removing temporary files from the HDD
void removeTempFiles(){
	char *command;

#if defined(__linux) || defined(__APPLE__)
	if(FileExists(TMP_FILE)){
		command = strdup("rm -f ");
		command = catMem(command, TMP_FILE);
		if (system(command) == -1){
			fprintf(stderr,"Error on removing %s.\n", TMP_FILE);
			return;
		}
		free(command);
	}
	if(FileExists(SCRIPT_PATH)){
	    	command = strdup("rm -f ");
		command = catMem(command, SCRIPT_PATH);
		if (system(command) == -1){
			fprintf(stderr,"Error on removing %s.\n", SCRIPT_PATH);
			return;
		}
		free(command);
	}
	if(FileExists(TRIVIAL_ENCODING_FILE)){
		command = strdup("rm -f ");
		command = catMem(command, TRIVIAL_ENCODING_FILE);
		if (system(command) == -1){
			fprintf(stderr,"Error on removing %s.\n", TRIVIAL_ENCODING_FILE);
			return;
		}
		free(command);
	}
	if(FileExists(CONSTRAINTS_FILE)){
		command = strdup("rm -f ");
		command = catMem(command, CONSTRAINTS_FILE);
		if (system(command) == -1){
			fprintf(stderr,"Error on removing %s.\n", CONSTRAINTS_FILE);
			return;
		}
		free(command);
	}
	if(FileExists(BOOL_PATH)){
		command = strdup("rm -f ");
		command = catMem(command, BOOL_PATH);
		if (system(command) == -1){
			fprintf(stderr,"Error on removing %s.\n", BOOL_PATH);
			return;
		}
		free(command);
	}
#else
	if(FileExists(TMP_FILE)){
	    	command = strdup("del ");
		command = catMem(command, TMP_FILE);
		if (system(command) == -1){
			fprintf(stderr,"Error on removing %s.\n", TMP_FILE);
			return;
		}
	}
	free(command);
	if(FileExists(SCRIPT_PATH)){
	    	command = strdup("del ");
		command = catMem(command, SCRIPT_PATH);
		if (system(command) == -1){
			fprintf(stderr,"Error on removing %s.\n", SCRIPT_PATH);
			return;
		}
		free(command);
	}
	if(FileExists(TRIVIAL_ENCODING_FILE)){
		command = strdup("del ");
		command = catMem(command, TRIVIAL_ENCODING_FILE);
		if (system(command) == -1){
			fprintf(stderr,"Error on removing %s.\n", TRIVIAL_ENCODING_FILE);
			return;
		}
		free(command);
	}
	if(FileExists(CONSTRAINTS_FILE)){
		command = strdup("del ");
		command = catMem(command, CONSTRAINTS_FILE);
		if (system(command) == -1){
			fprintf(stderr,"Error on removing %s.\n", CONSTRAINTS_FILE);
			return;
		}
		free(command);
	}
	if(FileExists(BOOL_PATH)){
		command = strdup("del ");
		command = catMem(command, BOOL_PATH);
		if (system(command) == -1){
			fprintf(stderr,"Error on removing %s.\n", BOOL_PATH);
			return;
		}
		free(command);
	}
#endif
	return;
}

void safe_exit(const char* message){
	fprintf(stderr,"%s\n", message);
	removeTempFiles();
	fflush(stderr);
	return;
}

/*PRINT BINARY FUNCTION*/
/*Following function print the binary representation of an integer number.*/
void print_binary(FILE *fp,int n, int bits){
	int *vett,j;

	vett = (int*)calloc(bits, sizeof(int));

	for(int i=0;i<bits; i++){
		if(n & 1) vett[i] = 1;
		n >>=1;
	}
	j = 0;
	for(int i=bits-1; i>= 0;i--){
		fprintf(fp,"%d", vett[i]);
	}
	if (fp != NULL)
		fprintf(fp," ");

	free(vett);

	return;
}

char* decimal_to_binary(int n, int bits){
	int *vett,j;
	char *binary = NULL;

	vett = (int*)calloc(bits, sizeof(int));
	binary = (char*) malloc(sizeof(char) * (bits+1));

	for(int i=0;i<bits; i++){
		if(n & 1) vett[i] = 1;
		n >>=1;
	}
	j = 0;
	for(int i=bits-1; i>= 0;i--){
		if(vett[i]) binary[j++] = '1';
		else	binary[j++] = '0';
	}
	binary[bits] = '\0';

	free(vett);

	return binary;
}

int temporary_files_creation(){
#if defined(__linux) || defined(__APPLE__)
	if (mkstemp(TRIVIAL_ENCODING_FILE) == -1){
		fprintf(stderr,"Error on opening trivial temporary file: %s.\n",
			TRIVIAL_ENCODING_FILE);
		return -1;
	}
	if (mkstemp(CONSTRAINTS_FILE) == -1){
		fprintf(stderr,"Error on opening constraint temporary file: %s.\n",
			CONSTRAINTS_FILE);
		return -1;
	}
	if (mkstemp(TMP_FILE) == -1){
		fprintf(stderr,"Error on opening temporary file: %s.\n", TMP_FILE);
		return -1;
	}
	if (mkstemp(SCRIPT_PATH) == -1){
		fprintf(stderr,"Error on opening temporary file: %s.\n", SCRIPT_PATH);
		return -1;
	}
	if (mkstemp(BOOL_PATH) == -1){
		fprintf(stderr,"Error on opening temporary file: %s.\n", BOOL_PATH);
		return -1;
	}
#else
	tmpnam(TRIVIAL_ENCODING_FILE);
	tmpnam(CONSTRAINTS_FILE);
	tmpnam(TMP_FILE);
	tmpnam(SCRIPT_PATH);
	tmpnam(BOOL_PATH);
#endif
	return 0;
}

/*LOGARITHM2 FUNCTION*/
/*Following function simply computes logarithm base 2 of input parameter.*/
int logarithm2(int n){
	return ceil(log2(n));
}

/*STRING COMPARE WITH DON'T CARE SUPPORT*/
/*Following function compare two bit strings, and returns 0 if the strings are equals
or 1 if they are different. It supports don't care conditions as well.*/
int strDCcmp(char* str1,char*str2,int bits){
	int diff = 0,i;
	for(i=0;i<bits;i++){
		if(str1[i] == '0' && str2[i] == '1')
			diff = 1;
		if(str1[i] == '1' && str2[i] == '0')
			diff = 1;
	}

	return diff;
}

/*CONVERSION BINARY -> INT FUNCTION*/
/*Following function simply converts char binary into int decimal.*/
int conv_int(char* string, int index){
	int num = 0,i,val = 1;

	for(i=strlen(string)-1;i>= 0;i--){
		if(string[i] == '1') num += val;
		if(string[i] == '-'){
			if(index != -1) DC_custom[index] = TRUE;
		} 
		if(string[i] == 'X'){
			DC = TRUE;
		}
		val *=2;
	}

	return num;
}

void int_to_string_DC(int bits, int index, int val, char *str){
	char *vett;
	int k=0,i;
	vett = (char*)malloc(bits * sizeof(char));

	for(i=0;i<bits; i++){
		if(val & 1) vett[i] = '1';
		else vett[i] = '0';
		val >>=1;
	}
	for(i= bits-1; i>= 0; i--){
			str[k++] = vett[i];
	}
	str[k] = '\0';

	for(i=0;i<bits; i++)
		if(manual_file[index][i] == '-')
			str[i] = '-';
	return;
}

void encodingReformat(encodingType encoding){

	if (encoding < 2) return;

	for(int i = 0; i < cpog_count; i++){
		int len = strlen(manual_file_back[i]);
		for(int j=0; j<len; j++){
			if(manual_file_back[i][j] == '-'){
				scenarioOpcodes[i][j] = '-';
			}
		}
	}

	return;
}

int set_opcodes(int cpog_count){
	opcodes = (BitType **) malloc (sizeof(BitType *) * cpog_count);

	for(int i=0; i<cpog_count; i++){
		opcodes[i] = (BitType *) malloc(sizeof(BitType) * bits);
		for(int j=0; j<bits; j++){
			switch(scenarioOpcodes[i][j]){
				case '0':
					opcodes[i][j] = ZERO;
					break;
				case '1':
					opcodes[i][j] = ONE;
					break;
				case '-':
					opcodes[i][j] = DONT_USE;
					break;
				default :
					return -1;
			}
		}
	}
	return  0;
}

int export_variables(encodingType encoding){

	encodingReformat(encoding);

	// set opcode length
	bits = scenarioOpcodes[0].length();

	// set opcodes
	if (set_opcodes(cpog_count) != 0){
		fprintf(stderr,"Conversion into BitType failed.\n");
		return -1;
	}

	counter = 1;
	

	return 0;
}

void opcodesForSynthesis(int index){

	for(int i = 0; i< cpog_count; i++){
		cons_perm[0][i] = perm[index][i];
	}
	return;
}

void loadScenarioOpcodes(int index){

	// for haskell
	if(!scenarioOpcodes.empty()) scenarioOpcodes.clear();
	scenarioOpcodes.resize(cpog_count);
	for(int i = 0; i < cpog_count; i++){
		char *numb = NULL;
		numb = decimal_to_binary(perm[index][i], bits);
		scenarioOpcodes[i] = string(numb);
		free(numb);
	}

	opcodesForSynthesis(index);

	return;
}

/*EVALUATION TRUTH TABLE FUNCTION*/
/*Following function evaluates if boolean function can be pre-set to 0 or 1
without passing trough espresso tool. It speeds up a lot the transformation process
from truth table to boolean functions.*/
int eval_function(char* truth,int n){
	int i;
	boolean one = FALSE,zero = FALSE;

	for(i=0;i<n;i++){
		if(truth[i] == '1') one = TRUE;
		if(truth[i] == '0') zero = TRUE;
	}

	if(one && zero) return 0;
	if(one && !zero) return 1;
	if( (!one && !zero)  || (!one && zero) ) return 2;

	return 0;
}

int heapVariablesAllocation(){
	eventNames_str = new string[eventsLimit];
	eventPredicates = new map<string, int>[eventsLimit];
	ev = new string*[eventsLimit];
	for(int i = 0; i<eventsLimit; i++){
		ev[i] = new string[predicatesLimit];
	}
	ee = new string*[eventsLimit];
	for(int i = 0; i<eventsLimit; i++){
		ee[i] = new string[eventsLimit];
	}
	vConditions = new string*[eventsLimit];
	for(int i = 0; i<eventsLimit; i++){
		vConditions[i] = new string[predicatesLimit];
	}
	aConditions = new string*[eventsLimit];
	for(int i = 0; i<eventsLimit; i++){
		aConditions[i] = new string[eventsLimit];
	}
	return 0;
}

int booleanFunctionsAllocation(){
	/*ALLOCATION MEMORY FOR ALL LOGIC FUNCTIONS CONSIDERED*/
	for(int i=0;i<nv; i++){
		for(int j=0;j<nv;j++){
			cpog[i][j].fun = (char**) malloc(sizeof(char*) * counter);
			cpog[i][j].fun_cond = (char**) malloc(sizeof(char*) * counter);
			if(cpog[i][j].fun == NULL || cpog[i][j].fun_cond == NULL)
				return -1;
			for(int k=0; k<counter; k++){
				cpog[i][j].fun[k] = NULL;
				cpog[i][j].fun_cond[k] = NULL;
			}
		}
	}
	return 0;
}

void freeBooleanFunctions(){
	for(int i = 0; i<nv; i++){
		for(int j = 0; j<nv; j++){
			if (cpog[i][j].fun != NULL){
				for(int k=0;k < counter; k++){
					if(cpog[i][j].fun[k] != NULL)
						free(cpog[i][j].fun[k]);
				}
				free(cpog[i][j].fun);
				cpog[i][j].fun = NULL;
			}
			if (cpog[i][j].fun_cond != NULL){
				for(int k=0;k < counter; k++){
					if(cpog[i][j].fun_cond[k] != NULL)
						free(cpog[i][j].fun_cond[k]);
				}
				free(cpog[i][j].fun_cond);
				cpog[i][j].fun_cond = NULL;
			}
		}
	}

	if(name_cond != NULL){
		for(int i = 0; i<n_cond; i++){
			if(name_cond[i] != NULL) free(name_cond[i]);
		}
		free(name_cond);
		name_cond = NULL;
		n_cond = 0;
	}
	if(perm != NULL) {
		for(long long int i = 0; i<num_perm; i++)
			if(perm[i] != NULL )free(perm[i]);
		free(perm);
		perm = NULL;
	}
	if(cons_perm != NULL) {
		if(cons_perm[0] != NULL )free(cons_perm[0]);
		free(cons_perm);
		cons_perm = NULL;
	}
	if(weights != NULL){
		free(weights);
		weights = NULL;
	}
	return;
}

void freeVariables(){
	if(manual_file != NULL){
		free(manual_file);
		manual_file = NULL;
	}
	if(manual_file_back != NULL){
		free(manual_file_back);
		manual_file_back = NULL;
	}
	if(custom_perm != NULL){
		free(custom_perm);
		custom_perm = NULL;
	}
	if(custom_perm_back != NULL){
		free(custom_perm_back);
		custom_perm_back = NULL;
	}
	if(opt_diff != NULL) {
		for(int i = 0; i<cpog_count; i++)
			if(opt_diff[i] != NULL) free(opt_diff[i]);
		free(opt_diff);
		opt_diff = NULL;
	}
	if(file_cons != NULL){
		free(file_cons);
		file_cons = NULL;
	}
	if(diff != NULL){
		for(int i = 0; i< len_sequence; i++)
			if(diff[i] != NULL) free(diff[i]);
		free(diff);
		diff = NULL;
	}
	if(opcodes != NULL){
		for(int i = 0; i < cpog_count; i++) 
			if(opcodes[i] != NULL) free(opcodes[i]);
		free(opcodes);
		opcodes = NULL;
	}
	if(enc != NULL){
		free(enc);
		enc = NULL;
	}
	if(sol != NULL){
		free(sol);
		sol = NULL;
	}
	cpog_count = 0;
	n = 0;
	len_sequence = 0;

	// Andrey's tool
	if(g != NULL){
		for(int i = 0; i<n; i++){
			if(g[i].e != NULL){
				for(int j=0; j<eventsLimit; j++)
					delete[] g[i].e[j];
				delete[] g[i].e;
				g[i].e = NULL;
			}
			if(g[i].v != NULL){
				delete[] g[i].v;
				g[i].v = NULL;
			}
			if(g[i].pred != NULL){
				delete[] g[i].pred;
				g[i].pred = NULL;
			}
		}
		free(g);
		g = NULL;
	}
	if(eventNames_str != NULL){
		delete[] eventNames_str;
		eventNames_str = NULL;
	}

	if (eventPredicates != NULL){
		delete[] eventPredicates;
		eventPredicates = NULL;
	}
	if(ev != NULL){
		for(int i = 0; i<eventsLimit; i++){
			delete[] ev[i];
		}
		delete[] ev;
		ev = NULL;
	}
	if(ee != NULL){
		for(int i = 0; i<eventsLimit; i++){
			delete[] ee[i];
		}
		delete[] ee;
		ee = NULL;
	}
	if(vConditions != NULL){
		for(int i = 0; i<eventsLimit; i++){
			delete[] vConditions[i];
		}
		delete[] vConditions;
		vConditions = NULL;
	}
	if(aConditions != NULL){
		for(int i = 0; i<eventsLimit; i++){
			delete[] aConditions[i];
		}
		delete[] aConditions;
		aConditions = NULL;
	}
	if( !eventNames.empty() ) eventNames.clear();
	if( !scenarioNames.empty() )scenarioNames.clear();
	if( !scenarioOpcodes.empty() )scenarioOpcodes.clear();
	if(!constraints.empty()) constraints.clear();
	if ( !encodings.empty() )encodings.clear();
	if ( !cgv.empty() ) cgv.clear();
	if ( !cge.empty() ) cge.clear();
	if ( !literal.empty() ) literal.clear();
	if ( !bestLiteral.empty() ) bestLiteral.clear();

	for(int i = 0; i<nv; i++){
		free(vertices[i]);
	}
	free(vertices);
	vertices = NULL;


	if( graphRead && cpog != NULL){
		for(int i = 0; i<nv; i++){
			for(int j = 0; j<nv; j++){
				if(cpog[i][j].type == 'e' || cpog[i][j].type == 'v'){
					if (cpog[i][j].source != NULL){
						free(cpog[i][j].source);
						cpog[i][j].source = NULL;
					}
					if (cpog[i][j].dest != NULL){
						free(cpog[i][j].dest);
						cpog[i][j].dest = NULL;
					}
					if (cpog[i][j].cond != NULL){
						free(cpog[i][j].cond);
						cpog[i][j].cond = NULL;
					}
					if (cpog[i][j].truth != NULL){
						free(cpog[i][j].truth);
						cpog[i][j].truth = NULL;
					}
					if (cpog[i][j].truth_cond != NULL){
						free(cpog[i][j].truth_cond);
						cpog[i][j].truth_cond = NULL;
					}
				}
			}
			if(cpog[i] != NULL) free(cpog[i]);
		}
		free(cpog);
		cpog = NULL;
	}
	return;
}

void resetVariables(){

	num_vert = 0;
	nv = 0;
	V = 0;
	n = 0;
	cpog_count = 0;
	nEquations = 0;
	len_sequence = 0;
	n_cond = 0;
	graphRead = FALSE;

	return;
}

int parse_area_result_abc(){

	FILE *fp = NULL;
	char string[50];
	char c;

	fp = fopen(TMP_FILE, "r");
	while( fscanf(fp,"%s", string) != EOF ){
		if(!strcmp(string, "TOTAL")){
			while((c = fgetc(fp)) != '=');
			if (fscanf(fp,"%d", &gates) != 1){
				return -1;
		
			}
			while((c = fgetc(fp)) != '=');
			if(fscanf(fp,"%f", &area) != 1){
				return -1;
		
			}
			break;
		} else {
			while((c = fgetc(fp)) != '\n');
		}
	}
	fclose(fp);
	
	return 0;
}

int encoding_memory_allocation(){

	int elements;
	int min_disp;

	// number of possible encoding
	tot_enc = 1;
	for(int i=0;i<bits;i++) tot_enc *= 2;

	// permutation or disposition?
	num_perm = 1;
	if (n == tot_enc){
		// permutation case
		if(!unfix && !SET){
			for(int i = 1; i< tot_enc; i++)
				num_perm *= i;
		}else{
			for(int i = 1; i<= tot_enc; i++)
				num_perm *= i;
		}
	}
	else{
		// disposition
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

	// space for codes available
	enc = (int*) calloc(tot_enc, sizeof(int));
	if( enc == NULL){
		return -1;
	}

	// First element is fixed
	if (!unfix && !SET)
		enc[0] = 1;

	sol = (int*) calloc(tot_enc, sizeof(int));
	if (sol == NULL){
		return -1;
	}	

	return -1;
}
