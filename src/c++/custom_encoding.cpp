/*READ ENCODING SET FUNCTION*/
/*This function reads the encoding set by designer in order to fix them.*/
int read_set_encoding(char *custom_file_name,int cpog_count, int *bits){
	FILE *fp = NULL;
	int i,k,b=0;
	char *number;
	char *tmp_str;
	char c;
	boolean acq = FALSE, freeCode = TRUE, dontUse = FALSE;

	fp = fopen(custom_file_name,"r");
 	while ( ( c = fgetc ( fp ) ) != '\n' ) b++;
	number = (char*) malloc(sizeof(char) * (b+2));
	fclose(fp);	

	fp = fopen(custom_file_name,"r");
	custom_perm = (int*) malloc(sizeof(int) * cpog_count);
	custom_perm_back = (int*) malloc(sizeof(int) * cpog_count);
	DC_custom = (boolean*) calloc(cpog_count,sizeof(boolean));

	for (i=0;i<cpog_count;i++){
		if( (fscanf(fp,"%s", number) == EOF)){
			fprintf(stderr,"Error: not enough encoding for this CPOG, they must be %d.\n",cpog_count);
			return 1;
		}

		freeCode = TRUE;
		if(number[0] != '/'){
			// RESET number of bits
			if(!acq){
				*bits = strlen(number);
				acq = TRUE;
				tot_enc = 1;
				for(k=0;k<(*bits);k++) tot_enc *= 2;
			}
			dontUse = FALSE;
			for(int j=0; j<(*bits) && !dontUse; j++){
				if(number[j] == '-'){
					dontUse = TRUE;
					DC_custom[i] = TRUE;
				}
			}
			for(int j=0; j<(*bits) && freeCode; j++){
				if(number[j] != 'X') freeCode = FALSE;
			}
			if(freeCode){
				custom_perm[i] = -1;
				custom_perm_back[i] = -1;
			} else{
				SET = TRUE;
				k = conv_int(number, i);
				custom_perm[i] = k;
				custom_perm_back[i] = k;
			}
		}
		else{
			custom_perm[i] = -1;
			custom_perm_back[i] = -1;
		}
	}
	bits_saved = *bits;
	fclose(fp);

	
	manual_file = (char**) malloc(sizeof(char*) * cpog_count);
	manual_file_back = (char**) malloc(sizeof(char*) * cpog_count);
	tmp_str = (char*) malloc(sizeof(char) * ((*bits)+1));
	for(i=0;i<cpog_count;i++){
		manual_file[i] = (char*) malloc(sizeof(char) * ((*bits)+1));
		manual_file_back[i] = (char*) malloc(sizeof(char) * ((*bits)+1));
	}


	if( (fp = fopen(custom_file_name,"r")) == NULL ){
		fprintf(stderr,"Error on opening manual file.\n");
		return 2;
	}

	for(i=0;i<cpog_count;i++){
		if(fscanf(fp,"%s",tmp_str) == EOF){
			fprintf(stderr,"Error on reading custom encodings.\n");
			return 3;
		}
		freeCode = TRUE;
		for(int j=0; j<(*bits) && freeCode; j++){
			if(tmp_str[j] != 'X') freeCode = FALSE;
		}
		if(freeCode) strcpy(manual_file[i], "/");
		else strcpy(manual_file[i], tmp_str);
		strcpy(manual_file_back[i],manual_file[i]);
	}

	free(tmp_str);
	free(number);

	fclose(fp);
	return 0;
	
}

int check_correctness(char *custom_file_name, int cpog_count, int tot_enc, int bits){

	int result = 0, *opcodes,i,k, it = 0, limit, res_back;
	opcodes = (int*) calloc(tot_enc, sizeof(int));
	char *number;
	boolean ins = FALSE;

	// INSERTING PREDEFINED OP-CODES
	for(i=0;i<cpog_count;i++){
		if(custom_perm[i] != -1 && !DC_custom[i]){
			result++;
		}
	}

	res_back = result;
	limit = 100000;
	while(it < limit){

		// BRING BACK RESULT TO INITIAL STATE
		result = res_back;
		ins = TRUE;

		for(i=0; i<cpog_count; i++){
			if(custom_perm[i] != -1){
				strcpy(manual_file[i],manual_file_back[i]);
				//printf("%s\n", manual_file[i]);
				for(k=0; k<bits; k++){
					// SUBSTITUTE DON'T CARES WITH VALUES
					if(manual_file[i][k] == 'X')
						manual_file[i][k] = (rand() % 2) ? '1': '0';
					// SUBSTITUTE NOT BITS WITH DON'T CARES
					if(manual_file[i][k] == '-')
						manual_file[i][k] = 'X';
				}
				custom_perm[i] = conv_int(manual_file[i], i);
			}
		}

		// SET OP-CODES ALL AVAILABLES
		for(i=0; i<tot_enc; i++)
			opcodes[i] = 0;

		// INSERTING OP-CODE ALREADY FIXED
		for(i=0; i<cpog_count; i++){
			if(custom_perm[i] != -1 && !DC_custom[i]){
				if(opcodes[custom_perm[i]] == 1){
					ins = FALSE;
					if(it > 1000){
						fprintf(stderr,"Op-code %s present multiple times.\n", manual_file_back[i]);
						return 2;
					}
				}
				opcodes[custom_perm[i]] = 1;
				
			}
		}

		// INSERTING OP-CODE WITH DON'T CARES
		for(i=0; i<cpog_count; i++){
			if(custom_perm[i] != -1 && DC_custom[i]){
				result++;
				for(k=0; k<tot_enc; k++){
					char *numb = NULL;
					numb = decimal_to_binary(k, bits);
					if(!strDCcmp(numb,manual_file[i],bits)){
						if(opcodes[k] == 1){
							ins = FALSE;
							if(it > 1000){
								char *numb2 = NULL;
								numb2 = decimal_to_binary(k, bits);
								fprintf(stderr,"Op-code %s and %s cannot be set at the same time.\n", manual_file_back[i], numb2);
								free(numb2);
								return 2;
							}
						}
						opcodes[k] = 1;
					}
					free(numb);
				}
			}

		}

		// VERIFYING ALL THE GRAPHS COULD BE ENCODED
		for(i=0;i<tot_enc && result < cpog_count;i++){
			if(opcodes[i] == 0){
				opcodes[i] = 1;
				result++;
			}
		}

		if(result != cpog_count && it > 1000){
			fprintf(stderr,"Not enough op-codes for all partial order graphs.\n");
			return 2;
		}

		// IF ALL GRAPHS ARE HAS BEEN ENCODED EXIT
		if(result == cpog_count && ins){
			for(i=0; i<cpog_count; i++){
				//printf("%s\n",manual_file[i]);
				custom_perm[i] = custom_perm_back[i];
				strcpy(manual_file[i],manual_file_back[i]);
			}
			return 0;
		}
		it++;
	}
	
	return 1;

}
