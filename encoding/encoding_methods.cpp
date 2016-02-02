/*******************************************************************************
*                          single-literal encoding                             *
*******************************************************************************/
int singleLiteralEncoding(int total){

	int L = 0, R = cgv.size() / 2, cnt = 1;
	while(R - L > 1)
	{
		int limit = (L + R) / 2;
	
		for(unsigned int i = 0; i < cgv.size(); i++) literal[i] = -1;
	
		printf(" [%d]", cnt++);
	
		bool res = false;
		res = encode(0, limit, 0);
	
		if (res)
		{
			bestLiteral = literal;
			R = limit;
		}
		else L = limit;
	}

	printf("DONE.\nThe best encoding uses %d operational variables:\n", R);
	fflush(stdout);

	scenarioOpcodes.resize(n);

	for(int i = 0; i < n; i++) for(int j = 0; j < R; j++) scenarioOpcodes[i] += "-";
    
	int k = 0;
	for(int i = 0; i < total; i++)
	if (!encodings[i].trivial)
	{
		int id = k * 2;
		int inv = 0;
	
		if (bestLiteral[id] == -1) inv = 1;
	
		printf("%s        ", cgv[id].c_str());
		if (inv) printf("!");
		printf("x[%d]\n", bestLiteral[id + inv]);
	
		encodings[i].literal = bestLiteral[id + inv];
		encodings[i].inverted = inv;
	
		for(int j = 0; j < n; j++) if (cgv[id][j] != '-') scenarioOpcodes[j][bestLiteral[id + inv]] = cgv[id + inv][j];

		k++;
	}

	for(int i = 0; i < total; i++)
	{
		string s = encodings[i].constraint;
		char tmp[10];
	
		string f = "";
		if (encodings[i].trivial)
		{
			f += '0' + encodings[i].constant;
		}
		else
		{
			sprintf(tmp, "x[%d]", encodings[i].literal);
			f = tmp;
			if (encodings[i].inverted) f = "!" + f;		
		}		
	
		for(unsigned int j = 0; j < constraints[s].size(); j++)
		{
			int a = constraints[s][j].first;
			int b = constraints[s][j].second;
		
			if (a < 0) vConditions[b][-a - 1] = f;
			else aConditions[a][b] = f;
		}
	}

	puts("\nVertex conditions:\n");

	for(int i = 0; i < V; i++)
	{
		string f = vConditions[i][0];
		map<string, int>::iterator p = eventPredicates[i].begin(), q = eventPredicates[i].end();
	
		int k = 1;
		while(p != q)
		{
			if (!alternative)
			{
				if (vConditions[i][k] != "1") f += " + " + vConditions[i][k] + " * " + (p->first);
				else f += " + " + (p->first);
				p++;
				k++;
			}
			else
			{
				if (vConditions[i][k] != "0") f += " * (" + vConditions[i][k] + " + " + (p->first) + ")";
				else f += " * " + (p->first);
				p++;
				k++;
			}
		}
		if (f.find("0 + ") == 0) f.erase(0, 4);
		if (f.find("1 * ") == 0) f.erase(0, 4);
		printf("%10s: %s\n", eventNames_str[i].c_str(), f.c_str());
	}

	puts("\nArc conditions:\n");

	for(int i = 0; i < V; i++)
	for(int j = 0; j < V; j++)
	if (i != j)
	{
		string f = aConditions[i][j];
		if (f == "0") continue;

		printf("%10s -> %-10s: %s\n", eventNames_str[i].c_str(), eventNames_str[j].c_str(), f.c_str());
	}

	return 0;
}

/*******************************************************************************
*                             sequential encoding                              *
*******************************************************************************/
int sequentialEncoding(){
	int bits = logarithm2(n);
	scenarioOpcodes.resize(n);
	
	for(int i = 0; i < n; i++){
		print_binary(NULL, i, bits);
		scenarioOpcodes[i] = string(numb);
	}

	return 0;
}

int satBasedEncoding(){

	return 0;
}

int randomEncoding(int cpog_count, int tot_enc,int bits){

	int min = MAX_WEIGHT,k;
	int i,j,c = 0,i_min,j_min,enc1 = 0,enc2 = 0,inc,p = 0,n = 1;
	int *full = NULL, *encod = NULL, **matrix_ass;
	int v_min_i[MAX_CPOG], v_min_j[MAX_CPOG];
	int *solution = NULL;
	boolean ins, out = FALSE;
	char *number;
#ifdef ACT_STOP
	int fails = 0;
#endif
	//ALLOC DATA STRUCTURE TO SUPPORT ENCODING GENERATION
	full = (int*) malloc(sizeof(int)*cpog_count);
	solution = (int*) malloc(sizeof(int)*cpog_count);
	encod = (int*) malloc(sizeof(int)*tot_enc);
	matrix_ass = (int**) malloc(sizeof(int*) * cpog_count);
	for(i=0;i<cpog_count;i++)
		matrix_ass[i] = (int*) calloc(cpog_count, sizeof(int));

	while(c < num_perm && out == FALSE){
		
		//RESET SUPPORT DATA STRUCTURES
		for(i=0;i<cpog_count;i++){
			full[i] = -1;
			solution[i] = -1;
		}
		for(i=0;i<tot_enc;i++){
			encod[i] = 0;
		}
		for(i=0;i<cpog_count-1;i++)
			for(j=i+1;j<cpog_count;j++)
				matrix_ass[i][j] = 0;
		inc = 0;
		

		// SET CUSTOM ENCODING
		if(SET){
			//SET VALUES FIXED BY USER
			for(i=0; i<cpog_count; i++){
				custom_perm[i] = custom_perm_back[i];
				strcpy(manual_file[i], manual_file_back[i]);
			}

			/*for(int y=0;y<cpog_count; y++){
				printf("%d %s\n",custom_perm[y], manual_file[y]);
			}
			printf("CPOG_ENCODED: ");
			for(i=0;i<cpog_count;i++) printf("%d ", full[i]);
			printf("\n");
			printf("SOLUTION: ");
			for(i=0;i<cpog_count;i++) printf("%d ", solution[i]);
			printf("\n");
			printf("OP-CODE USED: ");
			for(i=0;i<tot_enc;i++) printf("%d ", encod[i]);
			printf("\n");*/

			ins = FALSE;
			while(!ins){
				ins = TRUE;

				for(i=0; i<cpog_count; i++){
					if(custom_perm[i] != -1){
						strcpy(manual_file[i],manual_file_back[i]);
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

				// BRING BACK OP-CODES AVAILABLES
				for(i=0; i<tot_enc; i++)
					encod[i] = 0;

				// FIXED OP-CODES
				j = 0;
				for(i=0;i<cpog_count;i++){
					if(custom_perm[i] != -1 && !DC_custom[i]){
						if(encod[custom_perm[i]] == 1){
							ins = FALSE;
						}
						full[i] = 1;
						solution[i] = custom_perm[i];
						encod[custom_perm[i]] = 1;
						j++;
					}
				}

				// INSERTING OP-CODES WITH DON'T CARES
				for(i=0; i<cpog_count; i++){
					if(custom_perm[i] != -1 && DC_custom[i]){
						j++;
						full[i] = 1;
						custom_perm[i] = conv_int(manual_file[i], i);
						solution[i] = custom_perm[i];
						for(k=0; k<tot_enc; k++){
							print_binary(NULL,k, bits);
							number = numb;
							if(!strDCcmp(number,manual_file[i],bits)){
								if(encod[k] == 1){
									ins = FALSE;
								}
								encod[k] = 1;
							}
						}
					}
				}
			}


			// DEBUG PRINTING
			/*for(int y=0;y<cpog_count; y++){
				printf("%d %s\n",custom_perm[y], manual_file[y]);
			}
			printf("CPOG_ENCODED: ");
			for(i=0;i<cpog_count;i++) printf("%d ", full[i]);
			printf("\n");
			printf("SOLUTION: ");
			for(i=0;i<cpog_count;i++) printf("%d ", solution[i]);
			printf("\n");
			printf("OP-CODE USED: ");
			for(i=0;i<tot_enc;i++) printf("%d ", encod[i]);
			printf("\n");*/

			for(i=0; i<cpog_count; i++){
				strcpy(manual_file[i],manual_file_back[i]);
				custom_perm[i] = custom_perm_back[i];
			}


			//DM POSITIONS ALREADY CONSIDERED
			for(i=0;i< cpog_count-1;i++){
				for(p=i+1;p<cpog_count;p++){
					if(full[i] == 1 && full[p] == 1){
						matrix_ass[i][p] = 1;
					}
				}
			}
		}else if(!unfix){
			solution[0] = 0;
			full[0] = 1;
			encod[0] = 1;
			j = 1;
		}
		else{
			j = 0;
		}

		for( p= j; p<cpog_count ; p=(p+inc)){
			min = MAX_WEIGHT;
			//FIND MINIMUMs INSIDE DIFFERENCES MATRIX
			for(i=0;i<cpog_count-1;i++){
				for( j=(i+1) ; j<cpog_count ; j++ ){
					if(matrix_ass[i][j] == 0){
						if(opt_diff[i][j] < min){
							min = opt_diff[i][j];
							v_min_i[0] = i;
							v_min_j[0] = j;
							n = 1;
						}else if (opt_diff[i][j] == min){
							v_min_i[n] = i;
							v_min_j[n] = j;
							n++;
						}
					}
				}
			}
			
			//CHOOSE WHICH MINIMUMs TO USE RANDOMLY
			i = rand()%n;
			i_min = v_min_i[i];
			j_min = v_min_j[i];

			//DECODE IT WITH ENCODING WITH MINIMUM HAMMING DISTANCE
			//ACTUALLY AVAILABLE
			if(full[i_min] == -1 && full[j_min] == -1){
				random_opcode_choice_v2(encod,tot_enc,&enc1,&enc2,bits,2,solution,i_min,j_min,cpog_count);
				inc = 2;
			}else{
				if (full[i_min] == 1)	enc1 = solution[i_min];
				else	enc1 = solution[j_min];
				random_opcode_choice_v2(encod,tot_enc,&enc1,&enc2,bits,1,solution,i_min,j_min,cpog_count);
				inc = 1;
			}

			//SET CHOSEN ENCODINGS INSIDE CURRENT SOLUTION
			if(inc == 2){
				solution[i_min] = enc1;
				full[i_min] = 1;
				solution[j_min] = enc2;
				full[j_min] = 1;
			}
			else{
				if(full[i_min] == -1){
					solution[i_min] = enc2;
					full[i_min] = 1;
				}
				else{
					solution[j_min] = enc2;
					full[j_min] = 1;
				}
			}

			//REMOVE FROM MATRIX RESULTS ALREADY CONSIDERATED
			for(i=0;i< cpog_count-1;i++){
				for(j=i+1;j<cpog_count;j++){
					if(full[i] == 1 && full[j] == 1){
						matrix_ass[i][j] = 1;
					}
				}
			}
			//DEBUG PRINTING: current solution
			/*for(i=0;i<cpog_count;i++){
				printf("%d ", solution[i]);
			}
			printf("\n\n");*/
		}

		//CHECK RESULT WAS NOT ALREADY PRESENT
		//INSIDE ALL THE JUST GENERATED ENCODINGS
		ins = TRUE;
		for(i=0; i<c && ins == TRUE ;i++){
			n = 0;
			for(j=0;j<cpog_count;j++)
				if(perm[i][j] == solution[j])
					n++;
			if (n == cpog_count)
				ins = FALSE;
		}

		//IF IT'S NOT THE CASE ADD SOLUTION
		//TO SOLUTION SET
		if(ins){
			for(i=0;i<cpog_count;i++)
				perm[c][i] = solution[i];
			c++;
#ifdef ACT_STOP
			fails = 0;
#endif
		}
#ifdef ACT_STOP
		//IF SOLUTION ALREADY EXISTS INCREMENT A COUNTER
		else{
				fails++;
		}

		//IF COUNTER EXCEEDS PREDEFINED DEADLINE
		//STOP ENCODINGS GENERATION
		if(fails > MAX_FAILS){
			num_perm = c;
			printf("\nFunction was able to generate just %lld permutations.\n",num_perm);
			out = TRUE;
		}
#endif
	
		//SELECT ENCODING WITHOUT CHECKING
		//IF IT'S ALREADY PRESENT	
		/*for(i=0;i<cpog_count;i++)
			perm[c][i] = solution[i];
		c++;*/
	}
	counter = num_perm;

	//FREE DATA STRUCTURES JUST USED
	for(i=0;i<cpog_count;i++)
		free(matrix_ass[i]);
	free(matrix_ass);
	free(full);
	free(encod);

	return 0;
}

int heuristicEncoding(){

	return 0;
}

int exhaustiveEncoding(){

	return 0;
}
