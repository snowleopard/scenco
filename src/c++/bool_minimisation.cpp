char* parseAbcBoolEquation(){
	FILE *fAbc = NULL;
	char *equation, c;
	
	fAbc = fopen(BOOL_PATH, "r");

	// throw away first three lines
	for(int i = 0; i < 3; i++){
		while( (c = fgetc(fAbc)) != '\n');
	}
	// output name
	while((c = fgetc(fAbc)) != '=');

	// space
	c = fgetc(fAbc);

	equation = strdup("");
	while((c = fgetc(fAbc)) != ';'){
		equation = catChar(equation, c);
	}

	fclose(fAbc);
	return equation;
}

/*BOOLEAN FUNCTION*/
/*Following function uses espresso program (developed by Berkeley University) for finding
and minimising logic functions, with respect encoding considered.*/
int boolean_function(int co, char* abcPath){
	char *file_out, *command, *ss, c;
	int i=0,j=0,p=0,k=0,val;
	FILE *fp,*pp;

	//FILE_TEMP NAME
	file_out = strdup(TMP_FILE);

	command = abcCommandOutNull(abcPath);

	// building script
	if( (fp = fopen(SCRIPT_TMP, "w")) == NULL){
		fprintf(stderr,"Error preparing script for Abc synthesis.\n");
		return -1;
	}
	fprintf(fp, "read_pla %s\n", TMP_FILE);
	fprintf(fp, "write_eqn %s\n", BOOL_PATH);
	fclose(fp);

	if(!co){
		for(i=0; i<counter;i++){
			for(p=0;p<nv; p++){
				for(j=0;j<nv;j++){
					if( (val = eval_function(cpog[p][j].truth,cpog_count)) == 0 ){

						/*WRITING FILE CONTENT FOR ESPRESSO*/
						if( (fp = fopen(file_out,"w")) == NULL ){
							printf("Error on opening file.\n");
							return 1;
						}

						fprintf(fp,".i %d\n", bits); /*Number of inputs*/
						fprintf(fp,".o 1\n");
						fprintf(fp,".ilb "); /*Names of inputs*/
						for(k=0;k<bits;k++) fprintf(fp, "x_%d ", k);
						fprintf(fp,"\n");
						fprintf(fp, ".ob "); /*Names of outputs*/
						if(cpog[p][j].type == 'v')
							fprintf(fp,"%s ",cpog[p][j].source);
						else
							fprintf(fp, "%s>%s ",cpog[p][j].source,cpog[p][j].dest);

						fprintf(fp,"\n");

						if(tot_enc == cpog_count){
							for(k=0;k<cpog_count;k++){
								print_binary(fp,cons_perm[i][k], bits); /*Input encodings*/
								fprintf(fp," ");
								fprintf(fp, "%c",cpog[p][j].truth[k]);
								fprintf(fp,"\n");
							}
						}else{
							write_conditions(fp,cpog_count,i,p,j,bits,co);
						}
						fprintf(fp,".e");
						fclose(fp);

						/*fp = fopen(file_out, "r");
						while((c =fgetc(fp)) != EOF) printf("%c",c);
						fclose(fp);*/

						if( system(command) == -1){
							fprintf(stderr, "Error running Abc for\
								early synthesis: %s\n", command);
							return -1;
						}

						// reading boolean function from abc
						char *eq = parseAbcBoolEquation();
						cpog[p][j].fun[i] = strdup(eq);

					}else{
						if(val == 1)
							cpog[p][j].fun[i] = strdup("1");
						else
							cpog[p][j].fun[i] = strdup("0");
					}
				}
			}
		}
	} else{
		for(i=0; i<counter;i++){
			for(p=0;p<nv; p++){
				if(cpog[p][p].type == 'v' && cpog[p][p].condition){
					if( (val = eval_function(cpog[p][p].truth_cond,cpog_count)) == 0 ){
						/*WRITING FILE CONTENT FOR ESPRESSO*/
						if( (fp = fopen(file_out,"w")) == NULL ){
							printf("Error on opening file.\n");
							return 4;
						}

						fprintf(fp,".i %d\n", bits); /*Number of inputs*/
						fprintf(fp,".o 1\n"); /*Number of outputs*/
						fprintf(fp,".ilb "); /*Names of inputs*/
						for(k=0;k<bits;k++) fprintf(fp, "x_%d ", k);
						fprintf(fp,"\n");
						fprintf(fp, ".ob "); /*Names of outputs*/
						fprintf(fp,"c(%s) ",cpog[p][p].source);

						fprintf(fp,"\n");

						if(tot_enc == cpog_count){
							for(k=0;k<cpog_count;k++){
								print_binary(fp,cons_perm[i][k], bits); /*Input encodings*/
								fprintf(fp," ");
								fprintf(fp,"%c",cpog[p][p].truth_cond[k]);
								fprintf(fp,"\n");
							}
						}
						else
							write_conditions(fp,cpog_count,i,p,p,bits,co);
						fprintf(fp,".e");
						fclose(fp);

						if (system(command) == -1){
							fprintf(stderr, "Error running Abc for\
								early synthesis: %s\n", command);
							return -1;
						}

						// reading boolean function from abc
						char *eq = parseAbcBoolEquation();
						cpog[p][p].fun_cond[i] = strdup(eq);
					}else{
						if(val == 1)
							cpog[p][p].fun_cond[i] = strdup("1");
						else
							cpog[p][p].fun_cond[i] = strdup("0");

					}
				}
			}
		}
	}

	free(file_out);
	free(command);

	return 0;
}

int get_formulae_nodes(char *abcPath){

	int err;

	if(!graphRead){
		graphRead = TRUE;
		if( readingGraphStructure() != 0){
			fprintf(stderr, "Error reading graph structure.\n");
			return -1;
		}
	}

	if(booleanFunctionsAllocation() != 0){
		fprintf(stderr, "Error allocating memory for boolean functions.\n");
		return -1;
	}

	computeCodesAvailable();

	/*CONVERT TRUTH TABLES INTO BOOLEAN FUNCTION*/
	if((err = boolean_function(0, abcPath)!= 0)){
		fprintf(stderr,"Error on getting boolean function using\
			Abc. Error code: %d\n", err);
		return -1;
	}


	/*CONVERT TRUTH TABLES INTO BOOLEAN FUNCTION OF CONDITION ONLY*/
	if((err = boolean_function(1, abcPath)!= 0)){
		fprintf(stderr,"Error on getting boolean function using\
			Abc. Error code: %d\n", err);
		return -1;
	}

	//ACQUIRE NAMES OF CONDITIONS
	if(get_conditions_names()){
		fprintf(stderr,"Error on getting condition names from CPOG representation.\n");
		return -1;
	}

	return 0;
}
