int compute_HD(int n1,int i, int n2,int j,int bits,int cpog_count){
	int ones,bit_diff,q;
	char *number;

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
			print_binary(NULL,n2, bits);
			number = numb;
			ones = 0;
			char str[MAX_NAME];						
			int_to_string_DC(bits, i, n1, str);
			for(q = 0; q<bits; q++){
				if(str[q] == '0' && number[q] == '1')
					ones++;
				if(str[q] == '1' && number[q] == '0')
					ones++;
			}
			return ones;
		}
		if(!DC_custom[i] && DC_custom[j]){
			print_binary(NULL,n1, bits);
			number = numb;
			char str[MAX_NAME];							
			int_to_string_DC(bits, j, n2, str);
			for(q = 0; q<bits; q++){
				if(str[q] == '0' && number[q] == '1')
					ones++;
				if(str[q] == '1' && number[q] == '0')
					ones++;
			}
			return ones;
		}
		if(DC_custom[i] && DC_custom[j]){
			char str[MAX_NAME], str2[MAX_NAME];
			int_to_string_DC(bits, i, n1, str);					
			int_to_string_DC(bits, j, n2, str2);
			for(q = 0; q<bits; q++){
				if(str[q] == '0' && str2[q] == '1')
					ones++;
				if(str[q] == '1' && str2[q] == '0')
					ones++;
			}
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
