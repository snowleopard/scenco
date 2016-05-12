int compute_HD(int n1,int i, int n2,int j,int bits,int cpog_count){
	int ones,bit_diff,q;

	ones = 0;
	if(SET){
		if(!DC_custom[i] && !DC_custom[j]){
			bit_diff = n1 ^ n2;
			for(q = 0; q<bits; q++){
				if(bit_diff & 1) ones++;
				bit_diff >>= 1;
			}
			return ones;
		}
		if(DC_custom[i] && !DC_custom[j]){
			char *str, *numb;
			numb = decimal_to_binary(n2, bits);
			ones = 0;
			str = (char*) malloc(sizeof(char) * (bits +1));
			int_to_string_DC(bits, i, n1, str);
			for(q = 0; q<bits; q++){
				if(str[q] == '0' && numb[q] == '1')
					ones++;
				if(str[q] == '1' && numb[q] == '0')
					ones++;
			}
			free(str);
			free(numb);
			return ones;
		}
		if(!DC_custom[i] && DC_custom[j]){
			char *str, *numb;
			numb = decimal_to_binary(n1, bits);
			str = (char*) malloc(sizeof(char) * (bits +1));						
			int_to_string_DC(bits, j, n2, str);
			for(q = 0; q<bits; q++){
				if(str[q] == '0' && numb[q] == '1')
					ones++;
				if(str[q] == '1' && numb[q] == '0')
					ones++;
			}
			free(str);
			free(numb);
			return ones;
		}
		if(DC_custom[i] && DC_custom[j]){
			char *str;
			char *str2;
			str = (char*) malloc(sizeof(char) * (bits +1));
			str2 = (char*) malloc(sizeof(char) * (bits +1));
			int_to_string_DC(bits, i, n1, str);					
			int_to_string_DC(bits, j, n2, str2);
			for(q = 0; q<bits; q++){
				if(str[q] == '0' && str2[q] == '1')
					ones++;
				if(str[q] == '1' && str2[q] == '0')
					ones++;
			}
			free(str);
			free(str2);
			return ones;
		}
		
			
	}
	bit_diff = n1 ^ n2;
	for(q = 0; q<bits; q++){
		if(bit_diff & 1) ones++;
		bit_diff >>= 1;
	}
	return ones;
}

/*FUNCTION TO MINISIME*/
float  weight_function(int Mij,int HDij){
	return ((Mij-HDij) * (Mij - HDij));
}

double compute_weight(int cpog_count,int bits,int index){
	int i,j,ones;
	double wg = 0;

	for(i=0;i<cpog_count-1;i++){
		for(j=i+1;j<cpog_count;j++){
			//COMPUTE HAMMING DISTANCE
			ones = compute_HD(perm[index][i],i, perm[index][j],j,bits,cpog_count);
			wg += weight_function(opt_diff[i][j],ones);
		}
	}

	return wg;

}

/*AREA FUNCTION SSD*/
/*Following function computes both the maximum and minimum weight for each encoding permutation
following SSD criterion.*/
int heuristic_choice(encodingType encodingT){
        int ones = 0;
	long long int wg = 0;
	int minEnc = 0;
	int *iMin = NULL;
	
	maxW = -1;
	minW = numeric_limits<long long int>::max();

	if( counter == 0){
		fprintf(stderr,"No encodings match the custom opcodes fixed.\n");
		return -1;
	}

	if(encodingT == randomE){
		loadScenarioOpcodes(0);
		return 0;
	}

        for(int i = 0; i<counter; i++){
                wg = 0;
                for(int j = 0; j<cpog_count-1; j++)
                        for(int k = j+1; k < cpog_count; k++){
				//COMPUTE HAMMING DISTANCE
				ones = compute_HD(perm[i][j],j, perm[i][k],k,bits,cpog_count);
				wg += weight_function(opt_diff[j][k],ones);
			}
                weights[i] = wg;
                if(wg < minW) minW = wg;
                if(wg > maxW) maxW = wg;
        }

	for(int i = 0; i< counter; i++){

		if(weights[i] == minW){
			minEnc++;
			iMin = (int*) realloc (iMin, sizeof(int) * minEnc);
			iMin[minEnc-1] = i;
		}
	}

	int iRand = rand() % minEnc;
	free(iMin);

	loadScenarioOpcodes(iRand);

        return 0;
}

void allocateEncodingsSynthesis(){
	cons_perm = (int**) malloc(sizeof(int*));
	cons_perm[0] = (int*) malloc(sizeof(int) * cpog_count);
	return;
}

int allocate_encodings_space(int mem){

	num_perm = mem;
	counter = 0;
	
	perm = (int**) malloc(sizeof(int*) * mem);
	if ( perm == NULL){
		fprintf(stderr,"perm variable = null\n");
		return -1;
	}
	for(long long int i=0;i<mem;i++){
		perm[i] = (int*) malloc(n * sizeof(int));
		if (perm[i] == NULL){
			fprintf(stderr,"perm[%lld] = null\n",i);
			return -1;
		}
	}
	weights = (long long int *) calloc(mem, sizeof(long long int));
	allocateEncodingsSynthesis();

	return 0;
}

void synthesisSpaceSingleLiteral(){
	allocateEncodingsSynthesis();
	for(int i = 0; i < cpog_count; i++){
		boolean dontUsedBit = FALSE;
		for(int j=0; j< bits; j++){
			if(scenarioOpcodes[i][j] == '-')
				dontUsedBit = TRUE;
		}
		if(dontUsedBit) DC_custom[i] = TRUE;
		else DC_custom[i] = FALSE;
	
		char *cstr = new char[scenarioOpcodes[i].length() + 1];
		strcpy(cstr, scenarioOpcodes[i].c_str());
		int k = conv_int(cstr, -1);
		cons_perm[0][i] = k;
		delete [] cstr;
	}
	return;
}
