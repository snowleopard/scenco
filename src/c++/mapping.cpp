// disable warnings about constant chars
#pragma GCC diagnostic ignored "-Wwrite-strings"

/*DECIDE FUNCTION*/
/*Following function, simply check if a function is 0 or 1, if this is the case it mustn't
be written in the .prg file.*/
int decide(char* function){
	if(function[0] == '1')
		return 1;
	if(function[0] == '0')
		return 2;
	if(function == NULL)
		return 3;
	return 0;
}

/*FINAL EQUATIONS AND ABC FUNCTION*/
/*Following function write in some file in a format compatible with abc tool (Developed by Berkeley
University), in order to get statistics about area of encoder. Moreover it calls abc tool.*/
int equations_abc(int cpog_count, int bits){
	int i,c,k,j,min_bits = 1;
	float k2;
	boolean ins = FALSE;
	FILE *fp = NULL, *pp = NULL;
	char *file_name = NULL, *string, *line;
	char *command;

	if(decode_flag)
		min_bits = logarithm2(cpog_count);

	for(c=0;c<counter;c++){

		//OPNENING FILE
		if( (fp = fopen(BOOL_PATH,"w")) == NULL ){
			printf("NAME FILE: %s\n", BOOL_PATH);
			printf("Error on opening file.\n");
			return 1;
		}

		//INPUT NAMES
		fprintf(fp,"INORDER =");
		if(!decode_flag)
			for(k=0;k<bits;k++)
				fprintf(fp," x_%d",k);
		for(k=0;k<nv;k++)
			fprintf(fp," ACK_%s",cpog[k][k].source);
		for(k=0;k<n_cond;k++)
			fprintf(fp," %s",name_cond[k]);
		if(decode_flag){
			for(k=0;k<min_bits;k++)
				fprintf(fp," Y%d",k);
		}
		fprintf(fp,";\n");

		//OUTPUT NAMES
		fprintf(fp,"OUTORDER =");
		if(decode_flag)
			for(k=0;k<bits;k++)
				fprintf(fp," x_%d",k);
		for(k=0;k<nv;k++)
			fprintf(fp," REQ_%s",cpog[k][k].source);
		fprintf(fp,";\n");

		//DECODER
		if(decode_flag){
			for(i = 0;i<bits;i++){
				fprintf(fp,"x_%d = ", i);
				fprintf(fp,"%s;\n",decoder[i]);
			}
		}

		//FUNCTIONS
		equations = NULL;
		for(i=0;i<nv; i++){
			nEquations++;
			equations = (char **) realloc(equations, (nEquations) * sizeof(char*));
			equations[nEquations-1] = strdup("");
			equations[nEquations-1] = catMem(equations[nEquations-1], "REQ_");
			equations[nEquations-1] = catMem(equations[nEquations-1], cpog[i][i].source);
			equations[nEquations-1] = catMem(equations[nEquations-1], " = ");

	
			fprintf(fp,"REQ_%s = ", cpog[i][i].source);
			ins = FALSE;
			line = strdup("");
			for(j=0;j<nv;j++){
				string = strdup("");
				//CONDITIONS OF VERTICES
				if(j == i){
					if(cpog[j][i].condition){
						if( (!decide(cpog[j][i].fun[c]) || !decide(cpog[j][i].fun_cond[c])) && ins){
							string = strdup(" * ");
							line = catMem(line, string);
						}
						if (!decide(cpog[j][i].fun[c]) || !decide(cpog[j][i].fun_cond[c]))
							ins = TRUE;
						if(!decide(cpog[j][i].fun_cond[c]) && !decide(cpog[j][i].fun[c])){
							string = strdup("( ( (");
							string = catMem(string, cpog[j][i].fun_cond[c]);
							string = catMem(string, ") * (");
							string = catMem(string, cpog[j][i].cond);
							string = catMem(string, ") ) + (");
							string = catMem(string, cpog[j][i].fun[c]);
							string = catMem(string, ")) ");
							line = catMem(line, string);
						}
						if(decide(cpog[j][i].fun_cond[c]) && !decide(cpog[j][i].fun[c])){
							string = strdup("( (");
							string = catMem(string, cpog[j][i].cond);
							string = catMem(string, ") + (");
							string = catMem(string, cpog[j][i].fun[c]);
							string = catMem(string, ") ) ");
							line = catMem(line, string);
						}
						if(!decide(cpog[j][i].fun_cond[c]) && decide(cpog[j][i].fun[c])){
							string = strdup("( (");
							string = catMem(string, cpog[j][i].cond);
							string = catMem(string, ") * (");
							string = catMem(string, cpog[j][i].fun_cond[c]);
							string = catMem(string, ") ) ");
							line = catMem(line, string);
						}
						if(decide(cpog[j][i].fun_cond[c]) && decide(cpog[j][i].fun[c]))
							ins = FALSE;
					}else{	
						if(!decide(cpog[j][i].fun[c]) && ins){
							string = strdup(" * ");
							line = catMem(line, string);
						}			
						if(!decide(cpog[j][i].fun[c])){
							ins = TRUE;
							string = strdup("(");
							string = catMem(string, cpog[j][i].fun[c]);
							string = catMem(string, ") ");
							line = catMem(line, string);
						}
					}
				//CONDITIONS OF EDGES
				}else{
					if(decide(cpog[j][i].fun[c]) != 2){
						if(cpog[j][j].condition){
							if( (!decide(cpog[j][i].fun[c]) || !decide(cpog[j][j].fun[c]) || decide(cpog[j][j].fun_cond[c]) != 3) && ins){
								string = strdup(" * ");
								line = catMem(line, string);
							}
							if (!decide(cpog[j][i].fun[c]) || !decide(cpog[j][j].fun[c]) || decide(cpog[j][j].fun_cond[c]) != 3)
								ins = TRUE;

							if(!decide(cpog[j][i].fun[c]) && !decide(cpog[j][j].fun[c]) && !decide(cpog[j][j].fun_cond[c])){
								string = strdup("(ACK_");
								string = catMem(string, cpog[j][i].source);
								string = catMem(string, " + !((((");
								string = catMem(string, cpog[j][j].fun_cond[c]);
								string = catMem(string, ") * (");
								string = catMem(string, cpog[j][j].cond);
								string = catMem(string, ")) + (");
								string = catMem(string, cpog[j][j].fun[c]);
								string = catMem(string, ")) * (");
								string = catMem(string, cpog[j][i].fun[c]);
								string = catMem(string, ")))");
								line = catMem(line, string);
							}

							if(decide(cpog[j][i].fun[c]) && !decide(cpog[j][j].fun[c]) && !decide(cpog[j][j].fun_cond[c])){
								string = strdup("(ACK_");
								string = catMem(string, cpog[j][i].source);
								string = catMem(string, " + !(((");
								string = catMem(string, cpog[j][j].fun_cond[c]);
								string = catMem(string, ") * (");
								string = catMem(string, cpog[j][j].cond);
								string = catMem(string, ")) + (");
								string = catMem(string, cpog[j][j].fun[c]);
								string = catMem(string, ")))");
								line = catMem(line, string);
							}

							if(!decide(cpog[j][i].fun[c]) && decide(cpog[j][j].fun[c]) && !decide(cpog[j][j].fun_cond[c])){
								string = strdup("(ACK_");
								string = catMem(string, cpog[j][i].source);
								string = catMem(string, " + !(((");
								string = catMem(string, cpog[j][j].fun_cond[c]);
								string = catMem(string, ") * (");
								string = catMem(string, cpog[j][j].cond);
								string = catMem(string, ")) * (");
								string = catMem(string, cpog[j][i].fun[c]);
								string = catMem(string, ")))");
								line = catMem(line, string);
							}

							if(decide(cpog[j][i].fun[c]) && decide(cpog[j][j].fun[c]) && !decide(cpog[j][j].fun_cond[c])){
								string = strdup("(ACK_");
								string = catMem(string, cpog[j][i].source);
								string = catMem(string, " + !((");
								string = catMem(string, cpog[j][j].fun_cond[c]);
								string = catMem(string, ") * (");
								string = catMem(string, cpog[j][j].cond);
								string = catMem(string, ")))");
								line = catMem(line, string);
							}

							if(!decide(cpog[j][i].fun[c]) && !decide(cpog[j][j].fun[c]) && decide(cpog[j][j].fun_cond[c])){
								string = strdup("(ACK_");
								string = catMem(string, cpog[j][i].source);
								string = catMem(string, " + !((");
								string = catMem(string, cpog[j][j].fun[c]);
								string = catMem(string, ") * (");
								string = catMem(string, cpog[j][i].fun[c]);
								string = catMem(string, ")))");
								line = catMem(line, string);
							}

							if(decide(cpog[j][i].fun[c]) && !decide(cpog[j][j].fun[c]) && decide(cpog[j][j].fun_cond[c])){
								string = strdup("(ACK_");
								string = catMem(string, cpog[j][i].source);
								string = catMem(string, " + !(");
								string = catMem(string, cpog[j][j].fun[c]);
								string = catMem(string, "))");
								line = catMem(line, string);
							}
							if(!decide(cpog[j][i].fun[c]) && decide(cpog[j][j].fun[c]) && decide(cpog[j][j].fun_cond[c])){
								string = strdup("(ACK_");
								string = catMem(string, cpog[j][i].source);
								string = catMem(string, " + !(");
								string = catMem(string, cpog[j][i].fun[c]);
								string = catMem(string, "))");
								line = catMem(line, string);
							}

							if(decide(cpog[j][i].fun[c]) && decide(cpog[j][j].fun[c]) && decide(cpog[j][j].fun_cond[c])){
								ins = TRUE;
								string = strdup("(ACK_");
								string = catMem(string, cpog[j][i].source);
								string = catChar(string, ')');
								line = catMem(line, string);
							}
						}else{
							if(ins){
								string = strdup(" * ");
								line = catMem(line, string);
							}

							if (!decide(cpog[j][i].fun[c]) || !decide(cpog[j][j].fun[c]))
								ins = TRUE;

							if(!decide(cpog[j][i].fun[c]) && !decide(cpog[j][j].fun[c])){
								string = strdup("(ACK_");
								string = catMem(string, cpog[j][i].source);
								string = catMem(string, " + !((");
								string = catMem(string, cpog[j][i].fun[c]);
								string = catMem(string, ") * (");
								string = catMem(string, cpog[j][j].fun[c]);
								string = catMem(string, ")))");
								line = catMem(line, string);
							}

							if(decide(cpog[j][i].fun[c]) && !decide(cpog[j][j].fun[c])){
								string = strdup("(ACK_");
								string = catMem(string, cpog[j][i].source);
								string = catMem(string, " + !(");
								string = catMem(string, cpog[j][j].fun[c]);
								string = catMem(string, "))");
								line = catMem(line, string);
							}

							if(!decide(cpog[j][i].fun[c]) && decide(cpog[j][j].fun[c])){
								string = strdup("(ACK_");
								string = catMem(string, cpog[j][i].source);
								string = catMem(string, " + !(");
								string = catMem(string, cpog[j][i].fun[c]);
								string = catMem(string, "))");
								line = catMem(line, string);
							}

							if(decide(cpog[j][i].fun[c]) && decide(cpog[j][j].fun[c])){
								ins = TRUE;
								string = strdup("(ACK_");
								string = catMem(string, cpog[j][i].source);
								string = catChar(string, ')');
								line = catMem(line, string);
							}
						}
					}
				}
				free(string);
			}
			//printf("\n");
			line = catChar(line, ';');
			if(line[0] == ';'){
				free(line);
				line = strdup("1;");
			}
			fprintf(fp,"%s\n", line);
			equations[nEquations-1] = catMem(equations[nEquations-1], line);
			free(line);
		}
		
		//CLOSING FILE AND FREE NAME MEMORY
		fclose(fp);

	}
	return 0;
}

/*WRITE CONDITIONS FUNCTION*/
/*Following function is in charge of preparing file for abc tool, if encodings with don't care
conditions occur. It sets for all the possible bits configuration '-' don't care condition.*/
void write_conditions(FILE *fp,int cpog_count,int i,int p,int q,int bits,int co){
	int j,k;
	boolean ins = FALSE;

	if(!DC){
		for(j=0;j<tot_enc;j++){
			ins = FALSE;
			for(k = 0;k<cpog_count;k++){
				if(j == cons_perm[i][k]){
					print_binary(fp,cons_perm[i][k], bits); /*Input encodings*/
					fprintf(fp," ");
					if(co)
						fprintf(fp,"%c",cpog[p][q].truth_cond[k]);
					else
						fprintf(fp, "%c",cpog[p][q].truth[k]);
					fprintf(fp,"\n");
					ins = TRUE;
				}
			}
			if (!ins){
				print_binary(fp,j, bits); /*Input encodings*/
				fprintf(fp," ");
				fprintf(fp,"-");
				fprintf(fp,"\n");
			}
		}
	}else{
		if(!SET){
			for(j=0;j<tot_enc;j++){
				char *numb = NULL;
				ins = FALSE;
				numb = decimal_to_binary(j, bits);
				/*MOD !ins not present before*/
				for(k = 0;k<cpog_count;k++)
					if(!strDCcmp(numb,manual_file[k],bits)){
						print_binary(fp,j, bits); /*Input encodings*/
						fprintf(fp," ");
						if(co)
							fprintf(fp,"%c",cpog[p][q].truth_cond[k]);
						else
							fprintf(fp, "%c",cpog[p][q].truth[k]);
						fprintf(fp,"\n");
						ins = TRUE;
					}
				if (!ins){
					print_binary(fp,j, bits); /*Input encodings*/
					fprintf(fp," ");
					fprintf(fp,"-");
					fprintf(fp,"\n");
				}
				free(numb);
			
			}
		}else{
			for(j=0;j<tot_enc;j++){
				ins = FALSE;
				char *numb = NULL;
				numb = decimal_to_binary(j, bits);
				/*MOD !ins not present before*/
				for(k = 0;k<cpog_count;k++){
					if(!DC_custom[k]){
						if(j == cons_perm[i][k]){
							print_binary(fp,cons_perm[i][k], bits); /*Input encodings*/
							fprintf(fp," ");
							if(co)
								fprintf(fp,"%c",cpog[p][q].truth_cond[k]);
							else
								fprintf(fp, "%c",cpog[p][q].truth[k]);
							fprintf(fp,"\n");
							ins = TRUE;
						}
					}else{
						char *str;
						str = (char*) malloc(sizeof(char) * (bits+1));
						int_to_string_DC(bits,k,cons_perm[i][k], str);

						if(!strDCcmp(numb,str,bits)){
							print_binary(fp,j, bits); /*Input encodings*/
							fprintf(fp," ");
							if(co)
								fprintf(fp,"%c",cpog[p][q].truth_cond[k]);
							else
								fprintf(fp, "%c",cpog[p][q].truth[k]);
							fprintf(fp,"\n");
							ins = TRUE;
						}
					}
				}
				if (!ins){
					print_binary(fp,j, bits); /*Input encodings*/
					fprintf(fp," ");
					fprintf(fp,"-");
					fprintf(fp,"\n");
				}
				free(numb);
			
			}
		}
	}
	return;
}

int equations_abc_cpog_size(int cpog_count, int bits){
	int i,c,k,j;
	float k2;
	FILE *fp = NULL, *pp = NULL;
	char *file_name = NULL, *s;
	char *command;

	for(c=0;c<counter;c++){

		//OPNENING FILE
		if( (fp = fopen(BOOL_PATH,"w")) == NULL ){
			printf("NAME FILE: %s\n", BOOL_PATH);
			printf("Error on opening file.\n");
			return 1;
		}


		//INPUT NAMES
		fprintf(fp,"INORDER =");
		for(k=0;k<bits;k++)
			fprintf(fp," x_%d",k);
		for(k=0;k<n_cond;k++)
			fprintf(fp," %s",name_cond[k]);
		fprintf(fp,";\n");

		//OUTPUT NAMES
		fprintf(fp,"OUTORDER =");
		for(k=0;k<nv;k++)
			for(j=0; j<nv; j++)
				if(cpog[k][j].type == 'v'){
					if((cpog[k][j].fun[c][0] != '0' && cpog[k][j].fun[c][0] != '1') || cpog[k][j].condition){
						fprintf(fp," %s",cpog[k][j].source);
					}
				} else {
					if(cpog[k][j].fun[c][0] != '0' && cpog[k][j].fun[c][0] != '1'){
						fprintf(fp, " %s->%s", cpog[k][j].source, cpog[k][j].dest);
					}
				}
		fprintf(fp,";\n");

		//FUNCTIONS FOR CPOG MINIMISATION
		equations = NULL;
		for(i=0;i<nv; i++){
			for(j=0; j<nv; j++){
				nEquations++;
				equations = (char **) realloc(equations, (nEquations) * sizeof(char*));
				equations[nEquations-1] = strdup("");

				if(cpog[i][j].type == 'v'){
					if(cpog[i][j].condition){
						fprintf(fp,"%s = (%s) + ((%s) * (%s));\n",cpog[i][j].source,cpog[i][j].fun[c], cpog[i][j].cond,cpog[i][j].fun_cond[c]);
						equations[nEquations-1] = catMem(equations[nEquations-1], cpog[i][j].source);
						equations[nEquations-1] = catMem(equations[nEquations-1], "= (");
						equations[nEquations-1] = catMem(equations[nEquations-1], cpog[i][j].fun[c]);
						equations[nEquations-1] = catMem(equations[nEquations-1], ") + ((");
						equations[nEquations-1] = catMem(equations[nEquations-1], cpog[i][j].cond);
						equations[nEquations-1] = catMem(equations[nEquations-1], ") * (");
						equations[nEquations-1] = catMem(equations[nEquations-1], cpog[i][j].fun_cond[c]);
						equations[nEquations-1] = catMem(equations[nEquations-1], "));");
					}
					else
						if(cpog[i][j].fun[c][0] != '0' && cpog[i][j].fun[c][0] != '1'){
							fprintf(fp,"%s = (%s);\n",cpog[i][j].source,cpog[i][j].fun[c]);
							equations[nEquations-1] = catMem(equations[nEquations-1], cpog[i][j].source);
							equations[nEquations-1] = catMem(equations[nEquations-1], " = (");
							equations[nEquations-1] = catMem(equations[nEquations-1], cpog[i][j].fun[c]);
							equations[nEquations-1] = catMem(equations[nEquations-1], ");");
						}
				}
				else
					if(cpog[i][j].fun[c][0] != '0' && cpog[i][j].fun[c][0] != '1'){
						fprintf(fp, "%s->%s = (%s);\n", cpog[i][j].source, cpog[i][j].dest, cpog[i][j].fun[c]);
						equations[nEquations-1] = catMem(equations[nEquations-1], cpog[i][j].source);
						equations[nEquations-1] = catMem(equations[nEquations-1], "->");
						equations[nEquations-1] = catMem(equations[nEquations-1], cpog[i][j].dest);
						equations[nEquations-1] = catMem(equations[nEquations-1], " = (");
						equations[nEquations-1] = catMem(equations[nEquations-1], cpog[i][j].fun[c]);
						equations[nEquations-1] = catMem(equations[nEquations-1], ");");
					}
			}
		}
		
		//CLOSING FILE AND FREE NAME MEMORY
		fclose(fp);

		free(file_name);
	}

	return 0;
}

int writeMappingScript(char *techLibrary){
	FILE *fp = NULL;

	if( (fp = fopen(SCRIPT_PATH,"w")) == NULL ){
		printf("Error on opening script file.\n");
		return 2;
	}

	// script
	fprintf(fp,"read_eqn %s\n",BOOL_PATH);
	fprintf(fp,"read_library %s\n", techLibrary);
	fprintf(fp,"fraig_store; balance; rewrite; rewrite -z; balance; rewrite -z; balance; fraig_store; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance;  refactor -z; rewrite -z; balance; fraig_store; fraig_restore; map\n");
	fprintf(fp,"fraig_store; balance; rewrite; rewrite -z; balance; rewrite -z; balance; fraig_store; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance;  refactor -z; rewrite -z; balance; fraig_store; fraig_restore; map\n");
	fprintf(fp,"fraig_store; balance; rewrite; rewrite -z; balance; rewrite -z; balance; fraig_store; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance;  refactor -z; rewrite -z; balance; fraig_store; fraig_restore; map\n");
	fprintf(fp,"fraig_store; balance; rewrite; rewrite -z; balance; rewrite -z; balance; fraig_store; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance;  refactor -z; rewrite -z; balance; fraig_store; fraig_restore; map\n");
	fprintf(fp,"fraig_store; balance; rewrite; rewrite -z; balance; rewrite -z; balance; fraig_store; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance;  refactor -z; rewrite -z; balance; fraig_store; fraig_restore; map\n");
	fprintf(fp,"fraig_store; balance; rewrite; rewrite -z; balance; rewrite -z; balance; fraig_store; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance;  refactor -z; rewrite -z; balance; fraig_store; fraig_restore; map\n");
	fprintf(fp,"fraig_store; balance; rewrite; rewrite -z; balance; rewrite -z; balance; fraig_store; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance;  refactor -z; rewrite -z; balance; fraig_store; fraig_restore; map\n");
	fprintf(fp,"print_gates\n");
	fprintf(fp,"quit");
	fclose(fp);

	return 0;
}

int writeVerilogGenScript(char *techLibrary, char *vFile){
	FILE *fp = NULL;

	if( (fp = fopen(SCRIPT_PATH,"w")) == NULL ){
		printf("Error on opening script file.\n");
		return 2;
	}

	// script
	fprintf(fp,"read_eqn %s\n",BOOL_PATH);
	fprintf(fp,"read_library %s\n", techLibrary);
	fprintf(fp,"fraig_store; balance; rewrite; rewrite -z; balance; rewrite -z; balance; fraig_store; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance;  refactor -z; rewrite -z; balance; fraig_store; fraig_restore; map\n");
	fprintf(fp,"fraig_store; balance; rewrite; rewrite -z; balance; rewrite -z; balance; fraig_store; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance;  refactor -z; rewrite -z; balance; fraig_store; fraig_restore; map\n");
	fprintf(fp,"fraig_store; balance; rewrite; rewrite -z; balance; rewrite -z; balance; fraig_store; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance;  refactor -z; rewrite -z; balance; fraig_store; fraig_restore; map\n");
	fprintf(fp,"fraig_store; balance; rewrite; rewrite -z; balance; rewrite -z; balance; fraig_store; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance;  refactor -z; rewrite -z; balance; fraig_store; fraig_restore; map\n");
	fprintf(fp,"fraig_store; balance; rewrite; rewrite -z; balance; rewrite -z; balance; fraig_store; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance;  refactor -z; rewrite -z; balance; fraig_store; fraig_restore; map\n");
	fprintf(fp,"fraig_store; balance; rewrite; rewrite -z; balance; rewrite -z; balance; fraig_store; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance;  refactor -z; rewrite -z; balance; fraig_store; fraig_restore; map\n");
	fprintf(fp,"fraig_store; balance; rewrite; rewrite -z; balance; rewrite -z; balance; fraig_store; balance; rewrite; refactor; balance; rewrite; rewrite -z; balance;  refactor -z; rewrite -z; balance; fraig_store; fraig_restore; map\n");
	fprintf(fp,"write_verilog %s\n", vFile);
	fprintf(fp,"quit");
	fclose(fp);

	return 0;
}

char* abcCommandOutNull(char *abcPath){

	char *command = NULL;

#if defined(__linux) || defined(__APPLE__)
	command = strdup("./");
#else
	command = strdup("");
#endif
	command = catMem(command, abcPath);
	command = catMem(command, " < ");
	command = catMem(command, SCRIPT_PATH);
#if defined(__linux) || defined(__APPLE__)
	command = catMem(command, " 1>/dev/null 2>&1");
#else
	command = catMem(command, " >nul 2>&1");
#endif
	return command;
}

char* abcCommandOutTmp(char *abcPath){

	char *command = NULL;

#if defined(__linux) || defined(__APPLE__)
	command = strdup("./");
#else
	command = strdup("");
#endif
	command = catMem(command, abcPath);
	command = catMem(command, " < ");
	command = catMem(command, SCRIPT_PATH);
	command = catMem(command, " > ");
	command = catMem(command, TMP_FILE);
	command = catMem(command, " 2>&1");
	return command;
}
