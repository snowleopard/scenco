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

// removing temporary files from the HDD
void removeTempFiles(){
	char *command;

#ifdef __linux
	command = strdup("rm -f ");
	command = catMem(command, TMP_FILE);
	if (system(command) == -1){
		fprintf(stderr,"Error on removing %s.\n", TMP_FILE);
		return;
	}
	free(command);
    	command = strdup("rm -f ");
	command = catMem(command, SCRIPT_PATH);
	if (system(command) == -1){
		fprintf(stderr,"Error on removing %s.\n", SCRIPT_PATH);
		return;
	}
	free(command);
	command = strdup("rm -f ");
	command = catMem(command, TRIVIAL_ENCODING_FILE);
	if (system(command) == -1){
		fprintf(stderr,"Error on removing %s.\n", TRIVIAL_ENCODING_FILE);
		return;
	}
	free(command);
	command = strdup("rm -f ");
	command = catMem(command, CONSTRAINTS_FILE);
	if (system(command) == -1){
		fprintf(stderr,"Error on removing %s.\n", CONSTRAINTS_FILE);
		return;
	}
	free(command);
#else
    	command = strdup("del ");
	command = catMem(command, TMP_FILE);
	if (system(command) == -1){
		fprintf(stderr,"Error on removing %s.\n", TMP_FILE);
		return;
	}
	free(command);
    	command = strdup("del ");
	command = catMem(command, SCRIPT_PATH);
	if (system(command) == -1){
		fprintf(stderr,"Error on removing %s.\n", SCRIPT_PATH);
		return;
	}
	free(command);
	command = strdup("del ");
	command = catMem(command, TRIVIAL_ENCODING_FILE);
	if (system(command) == -1){
		fprintf(stderr,"Error on removing %s.\n", TRIVIAL_ENCODING_FILE);
		return;
	}
	free(command);
	command = strdup("del ");
	command = catMem(command, CONSTRAINTS_FILE);
	if (system(command) == -1){
		fprintf(stderr,"Error on removing %s.\n", CONSTRAINTS_FILE);
		return;
	}
	free(command);
#endif
	return;
}

int temporary_files_creation(){
#ifdef __linux
	if (mkstemp(TRIVIAL_ENCODING_FILE) == -1){
		fprintf(stderr,"Error on opening trivial temporary file: %s.\n",
			TRIVIAL_ENCODING_FILE);
		removeTempFiles();
		return -1;
	}
	if (mkstemp(CONSTRAINTS_FILE) == -1){
		fprintf(stderr,"Error on opening constraint temporary file: %s.\n",
			CONSTRAINTS_FILE);
		removeTempFiles();
		return -1;
	}
	if (mkstemp(TMP_FILE) == -1){
		fprintf(stderr,"Error on opening temporary file: %s.\n", TMP_FILE);
		removeTempFiles();
		return -1;
	}
	if (mkstemp(SCRIPT_PATH) == -1){
		fprintf(stderr,"Error on opening temporary file: %s.\n", SCRIPT_PATH);
		removeTempFiles();
		return -1;
	}
#else
	tmpnam (TRIVIAL_ENCODING_FILE);
	tmpnam (CONSTRAINTS_FILE);
	tmpnam (TMP_FILE);
	tmpnam (SCRIPT_PATH);
	tmpnam (LOG);
#endif
	return 0;
}

/*LOGARITHM2 FUNCTION*/
/*Following function simply computes logarithm base 2 of input parameter.*/
int logarithm2(int n){
	int logval = 0, i = 1,j;

	for(j=0;j<MAX_LOG;j++){
		i *= 2;
		if (n == i)
			logval = -1;
	}

	/*DEBUG PRINTING*/
	//printf("logval: %d", logval);

	while(n){
		logval++;
		n >>= 1;
	}
	return logval;
}

/*PRINT BINARY FUNCTION*/
/*Following function print the binary representation of an integer number.*/
void print_binary(FILE *fp,int n, int bits){
	int i, *vett,j;
	vett = (int*)calloc(bits, sizeof(int));
	//char *number;

	numb = (char*) malloc(sizeof(char) * MAX_NAME);
	//printf("%d - ",n);
	/*DEBUG PRINTING: int number*/
	//if(fp == stdout)	
	//	fprintf(fp,"(%d) ", n);

	for(i=0;i<bits; i++){
		if(n & 1) vett[i] = 1;
		n >>=1;
	}
	j = 0;
	for(i=bits-1; i>= 0;i--){
		if(fp != NULL)
			fprintf(fp,"%d", vett[i]);
		if(vett[i]) numb[j++] = '1';
		else	numb[j++] = '0';
	}
	numb[bits] = '\0';

	//printf("%s\n",number);

	if (fp != NULL)
		fprintf(fp," ");

	return;
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
			DC_custom[index] = TRUE;
		}
		if(string[i] == 'X'){
			DC = TRUE;
		}
		val *=2;
	}

	/*if(cnt != 0)
		*used_bits = strlen(string) - cnt;*/


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

void encodingReformat(int cpog_count, encodingType encoding){

	if (encoding < 3) return;

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
