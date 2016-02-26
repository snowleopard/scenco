/*******************************************************************************
*                          single-literal encoding                             *
*******************************************************************************/
int singleLiteralEncoding(int total){

	int L = 0, R = cgv.size() / 2, cnt = 1;
	while(R - L > 1)
	{
		int limit = (L + R) / 2;
	
		for(unsigned int i = 0; i < cgv.size(); i++) literal[i] = -1;
	
		//printf(" [%d]", cnt++);
	
		bool res = false;
		res = encode(0, limit, 0);
	
		if (res)
		{
			bestLiteral = literal;
			R = limit;
		}
		else L = limit;
	}

	//printf("DONE.\nThe best encoding uses %d operational variables:\n", R);

	scenarioOpcodes.resize(n);
	clear_scenarios();

	for(int i = 0; i < n; i++) for(int j = 0; j < R; j++) scenarioOpcodes[i] += "-";
    
	int k = 0;
	for(int i = 0; i < total; i++)
	if (!encodings[i].trivial)
	{
		int id = k * 2;
		int inv = 0;
	
		if (bestLiteral[id] == -1) inv = 1;
	
		//printf("%s        ", cgv[id].c_str());
		//if (inv) printf("!");
		//printf("x[%d]\n", bestLiteral[id + inv]);
	
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

	//printf("\nVertex conditions:\n");

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
		//printf("%10s: %s\n", eventNames_str[i].c_str(), f.c_str());
	}

	//printf("\nArc conditions:\n");

	for(int i = 0; i < V; i++)
	for(int j = 0; j < V; j++)
	if (i != j)
	{
		string f = aConditions[i][j];
		if (f == "0") continue;

		//printf("%10s -> %-10s: %s\n", eventNames_str[i].c_str(), eventNames_str[j].c_str(), f.c_str());
	}

	return 0;
}

/*******************************************************************************
*                             sequential encoding                              *
*******************************************************************************/
int sequentialEncoding(){
	int bits = logarithm2(n);
	clear_scenarios();
	scenarioOpcodes.resize(n);
	
	for(int i = 0; i < n; i++){
		char *numb = NULL;
		numb = decimal_to_binary(i, bits);
		scenarioOpcodes[i] = string(numb);
		perm[0][i] = i;
		free(numb);
	}


	return 0;
}

/*******************************************************************************
*                               random encoding                                *
*******************************************************************************/
void random_opcode_choice_v2(int *encod,int tot_enc,int *enc1,int *enc2,int bits,int sel,int *sol,int i_min,int j_min,int cpog_count){
	int i,j,l;
	int r,p,where;
	int *vi, *vj;
	
	vi = (int *) malloc(sizeof(int) * (tot_enc * tot_enc));
	vj = (int *) malloc(sizeof(int) * (tot_enc * tot_enc));

	//IF SEL == 2, WE NEED 2 ENCODINGS OWNED BY TWO CPOG
	if(sel == 2){
		
		//LOOK FOR ENCDODING WITH MINIMUM HAMMING DISTANCE
		//AMONG AVAILABLE ONES
		r = 0;
		for(i=0;i<tot_enc-1;i++){
			for(j=i+1;j<tot_enc;j++){
				if(encod[i] == 0 && encod[j] == 0){
					sol[i_min] = i;
					sol[j_min] = j;
					vi[r] = i;
					vj[r++] = j;
				}
			}
		}
		//IF JUST A COUPLE OF ENCODING
		//RETURN THAT RESULT
		if(r == 1){
			encod[vi[0]] = 1;
			encod[vj[0]] = 1;
			(*enc1) = vi[0];
			(*enc2) = vj[0];
		}
		//OTHERWISE SELECT WHICH
		//COUPLE OF ENCODINGS CONSIDER
		//BY MAXIMISING FUNCTION
		else{
			l = rand() % r;
			sol[i_min] = vi[l];
			sol[j_min] = vj[l];
			encod[vi[l]] = 1;
			encod[vj[l]] = 1;
			(*enc1) = vi[l];
			(*enc2) = vj[l];
		}
	}
	//IF SEL == 1, ONE OF TWO CPOG HAS BEEN ALREADY ENCODED
	else{
		//SET THAT ENCODING
		i = (*enc1);
		if(sol[i_min] == -1){
			where = 0;
		}
		else{
			where = 1;
		}
		//AND PICK UP ANOTHER ENCODING MINIMISES
		//HAMMING DISTANCE WITH PREVIOUS ONE
		r = 0; // MOD
		for(j = 0;j<tot_enc;j++){
			if(j != i && encod[j] == 0){
		
		//CHECK THE ENCODING MINIMISING FUNCTION
				if(where == 0)	sol[i_min] = j;
				else	sol[j_min] = j;
					vj[r++] = j;
			}
		}
		
		//IF JUST ONE ENCODING EXISTS
		//PICK IT UP
		if(r == 1){
			encod[vj[0]] = 1;
			(*enc2) = vj[0];
		}
		//OTHERWISE PICK ENCODING
		//WHICH WHICH MAXIMISE FUNCTION
		else{
			p = rand() % r;
			if(where == 0)	sol[i_min] = vj[p];
			else	sol[j_min] = vj[p];
			encod[vj[p]] = 1;
			(*enc2) = vj[p];
		}
	}

	free(vi);
	free(vj);

	return;
}


int randomEncoding(){

	long long int min = numeric_limits<long long int>::max(),k;
	int i,j,c = 0,i_min,j_min,enc1 = 0,enc2 = 0,inc,p = 0,n = 1;
	int *full = NULL, *encod = NULL, **matrix_ass;
	int *v_min_i = NULL, *v_min_j = NULL;
	int *solution = NULL;
	boolean ins, out = FALSE;

	int fails = 0;

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
							char *numb = NULL;
							numb = decimal_to_binary(k, bits);
							if(!strDCcmp(numb,manual_file[i],bits)){
								if(encod[k] == 1){
									ins = FALSE;
								}
								encod[k] = 1;
							}
							free(numb);
						}
					}
				}
			}

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
			min = numeric_limits<long long int>::max();
			v_min_i = NULL;
			v_min_j = NULL;
			//FIND MINIMUMs INSIDE DIFFERENCES MATRIX
			for(i=0;i<cpog_count-1;i++){
				for( j=(i+1) ; j<cpog_count ; j++ ){
					if(matrix_ass[i][j] == 0){
						if(opt_diff[i][j] < min){
							min = opt_diff[i][j];
							n = 1;
							if(v_min_i != NULL && v_min_j != NULL){
								free(v_min_i);	
								v_min_i = NULL;
								free(v_min_j);	
								v_min_j = NULL;
							}
							v_min_i = (int*) realloc (v_min_i, sizeof(int) * n);
							v_min_j = (int*) realloc (v_min_j, sizeof(int) * n);
							v_min_i[0] = i;
							v_min_j[0] = j;
						}else if (opt_diff[i][j] == min){
							n++;
							v_min_i = (int*) realloc (v_min_i, sizeof(int) * n);
							v_min_j = (int*) realloc (v_min_j, sizeof(int) * n);
							v_min_i[n-1] = i;
							v_min_j[n-1] = j;
							
						}
					}
				}
			}

			//CHOOSE WHICH MINIMUMs TO USE RANDOMLY
			i = rand()%n;
			i_min = v_min_i[i];
			j_min = v_min_j[i];
			if(v_min_i != NULL) free(v_min_i);
			if(v_min_j != NULL) free(v_min_j);
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

			fails = 0;

		}

		//IF SOLUTION ALREADY EXISTS INCREMENT A COUNTER
		else{
				fails++;
		}

		//IF COUNTER EXCEEDS PREDEFINED DEADLINE
		//STOP ENCODINGS GENERATION
		if(fails > 100){
			num_perm = c;
			out = TRUE;
		}

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

/*SIMULATED ANNEALING*/
/*This function tunes the solution by using simulated annealing method.*/
int start_simulated_annealing(){
	int i,m,n,tmp,start,it;
	double proba;
	double alpha =0.996;
	double temperature;
	double epsilon = 0.1;
	double delta;
	double weight_current, weight_next;

	if(unfix)	start = 0;
	else	start = 1;

	int cn = 0;
	if(SET){
		for(i=0; i<cpog_count;i++)
			if(custom_perm[i] != -1) cn++;
	}
	if(cn < cpog_count -4)
	for(i=0;i<num_perm;i++){
		temperature = 10.0;
		it = 0;

		weight_current = compute_weight(cpog_count, bits, i);

		while(temperature > epsilon){
			
			//COMPUTE NEXT SOLUTION
			if(SET){
				do{
					m = (rand() % (cpog_count - start)) + start;
					n = (rand() % (cpog_count - start)) + start;
				}while(m == n || custom_perm[m] != -1 || custom_perm[n] != -1);	
			}else{
				do{
					m = (rand() % (cpog_count - start)) + start;
					n = (rand() % (cpog_count - start)) + start;
				}while(m == n);
			}		
			tmp = perm[i][m];
			perm[i][m] = perm[i][n];
			perm[i][n] = tmp;

			//COMPUTE COST FUNCTION FOR NEXT SOLUTION
			weight_next = compute_weight(cpog_count, bits, i);

			//COMPARE COST FUNCTIONS
			delta = weight_next -weight_current;

			if(delta < 0){
				//KEEP NEXT SOLUTION
				weight_current = weight_next;
			}else{
				proba = (rand() * 1.00) / RAND_MAX;
				//printf("RAND = %f, %d\n",proba,RAND_MAX);

				//KEEP NEXT SOLUTION IF PROB LESS THAN
				//EXP(-DELTA/TEMPERATURE)
				if(proba < exp(-delta/temperature)){
					weight_current = weight_next;
				}else{
					//KEEP PREVIOUS SOLUTION
					tmp = perm[i][m];
					perm[i][m] = perm[i][n];
					perm[i][n] = tmp;
				}

			}
			
			//COOLING PROCESS
			temperature *=alpha;
			it++;
		}

	}
	
	counter = num_perm;

	return 0;
}

/*PERMUTATION FUNCTION*/
/*Following function finds all possible permutations for all available encoding
for each CPOG.*/
void exhaustiveEncoding(int *sol,int k,int *enc,int n, int tot){
	long int i;
	if(counter >= num_perm)
		return;
	if(k == n-1){
		for(i = 0; i<n; i++)
			perm[counter][i] = sol[i];
		counter++;
		
	}
	else{
		for(i = 0; i < tot; i++)
			if(!enc[i]){
				sol[k+1] = i;
				enc[i] = 1;
				exhaustiveEncoding(sol, k+1, enc, n, tot);
				enc[i] = 0;
			}
	}
	return;
}

/*FILTER FUNCTION*/
/*This function filter the encodings generated with the Exhaustive search approach
with the custom encoding set by the user. Finally in the perm array will be present 
just the encodings with are not filtered by this function.*/
int filter_encodings(int n_cpog, int bits, int tot_enc){
	int i = 0;
	int j = 0;
	int k = 0;
	int index_filter = 0;
	int present = 0, *opcodesF;
	char *number;
	boolean out = FALSE;

	opcodesF = (int*) calloc(tot_enc, sizeof(int));

	// LOOP OVER EACH ENCODING GENERATED WITH THE EXHAUSTIVE SEARCH
	for(i = 0; i < num_perm;  i++) {
		
		// INITIALISING SUPPORT VARIABLES
		// drop out the encoding, if it does not fit the customisation
		out = FALSE;
		// opcodes already used for DC conditions
		for(j=0; j<tot_enc; j++)
			opcodesF[j] = 0;

		// custom opcodes
		for(j=0; j<n_cpog; j++){
			strcpy(manual_file[j],manual_file_back[j]);
			opcodesF[perm[i][j]] = 1; //opcodes used by the encoding
		}

		// CHECK FOR FIXED BITS
		for (j=0; j<n_cpog && out == FALSE; j++){

			// OPCODE FIXED
			if(custom_perm[j] != -1){
				char *numb = NULL;
				numb = decimal_to_binary(perm[i][j], bits);
				if( strDCcmp(numb, manual_file[j], bits) ){
					out = TRUE;
				}
				free(numb);
			}
		}

		// CHECK FOR RESERVED OPCODES
		for (j=0; j<n_cpog && out == FALSE; j++){

			present = 0;
		
	
			// OPCODE CONTAINS AT LEAST ONE RESERVED BIT
			if (DC_custom[j] == TRUE &&  out == FALSE){

				// CONVERT OPCODE INT INTO A STRING
				char *numb = NULL;
				numb = decimal_to_binary(perm[i][j], bits);
				number = numb;

				// SUBSTITUTE THE X BIT OF THE OPCODE WITH THE REAL VALUES GENERATED
				for(k=0; k<bits; k++){
					// SUBSTITUTE DON'T CARES (X) WITH VALUES
					if(manual_file[j][k] == 'X')
						manual_file[j][k] = number[k];
					// SUBSTITUTE RESERVED BITS (-) WITH DON'T CARES (X)
					if(manual_file[j][k] == '-')
						manual_file[j][k] = 'X';
				}

				// CHECK THAT EACH OPCODE WITH DON'T CARE REPRESENTS ONE AND ONLY ONE PO
				for(k=0; k<tot_enc;k++){
					char *numb = NULL;
					numb = decimal_to_binary(k, bits);

					if( !strDCcmp(numb, manual_file[j], bits) && opcodesF[k] == 1){
						present++;
					}
					free(numb);
				}

				(present > 1) ? out =  TRUE : FALSE;
			}
		}

		// IF ENCODING FITS THE CUSTOMISATION INSERT INTO SET OF SOLUTIONS
		if(out == FALSE){
			for(k=0; k<n_cpog; k++){
				perm[index_filter][k] = perm[i][k];
			}
			index_filter++;
		}
	}

	num_perm = index_filter;
	counter = index_filter;

	return 0;
}

