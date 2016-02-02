int getEventID(string name)
{
	if (eventNames.count(name) == 1) return eventNames[name];
	eventNames_str[V] = name;
	eventNames[name] = V++;
	return V - 1;
}

string getPredicateName(int event, int id)
{
	map<string, int>::iterator p = eventPredicates[event].begin(), q = eventPredicates[event].end();
	while(p != q)
	{
		int pid = p->second;
		if (id == pid) return p->first;
		p++;
	}
	return "";
}

bool ME(int scenario, int event1, int event2)
{
	if (!g[scenario].v[event1]) return true;
	if (!g[scenario].v[event2]) return true;
	if (!g[scenario].pred[event1]) return false;
	if (!g[scenario].pred[event2]) return false;
	string p1 = getPredicateName(event1, g[scenario].pred[event1]);
	string p2 = getPredicateName(event2, g[scenario].pred[event2]);
	if (p1 == "!" + p2) return true;
	if (p2 == "!" + p1) return true;
	return false;
}

int getPredicateID(int event, string name)
{
	if (eventPredicates[event].count(name) == 0)
	{
		int new_id = eventPredicates[event].size() + 1;
		eventPredicates[event][name] = new_id;	
	}

	return eventPredicates[event][name];
}

bool check(string s)
{
	for(unsigned int i = 0; i < s.size(); i++)
		if (!isalpha(s[i]) && !isdigit(s[i]) && s[i] != '_' && s[i] != '-') return false;
	return true;
}

bool readScenario()
{
	while(1)
	{
		if(fgets(s, sizeof(s), stdin) == NULL){
			printf("error on reading the file\n");
			return false;
		}
		istringstream str(s);
		string from, to;
		
		if (!(str >> from)) continue;
		if (from == ".end") break;
		if (from[0] == ':')
		{
			if (!(str >> to))
			{
				printf("Cannot parse predicate '%s'\n", s);
				return false;
			}
			int vid = getEventID(to);
			if (!g[n].v[vid] || g[n].pred[vid])
			{
				printf("Incorrect predicate '%s'\n", to.c_str());
				return false;
			}
			from.erase(0, 1);
			g[n].pred[vid] = getPredicateID(vid, from);
			continue;
		}
		if (!check(from))
		{
			printf("Cannot parse event '%s'\n", from.c_str());
			return false;
		}
		if (!(str >> to))
		{
			g[n].v[getEventID(from)] = 1;
			continue;
		}
		int fromID = getEventID(from), toID;
		
		g[n].v[fromID] = 1;
		
		do
		{
			if (!check(to))
			{
				printf("Cannot parse event '%s'\n", to.c_str());
				return false;
			}
			
			toID = getEventID(to);
			g[n].v[toID] = 1;
			g[n].e[fromID][toID] = 1;
			
			fromID = toID;
		} while(str >> to);
	}
	
	if (!g[n].transitiveClosure())
	{
		puts("a cyclic event dependency detected!");
		return false;
	}
	
	int nv = 0, ntran = 0, nnontran = 0, npred = 0;
	
	for(int i = 0; i < eventsLimit; i++)
	{
		if (g[n].v[i]) nv++;
		if (g[n].pred[i]) npred++;
	}
	for(int i = 0; i < eventsLimit; i++)
	for(int j = 0; j < eventsLimit; j++)
	{
		if (g[n].e[i][j] == 1) nnontran++;
		if (g[n].e[i][j] == 2) ntran++;
	}
	
	printf("%d events, %d dependencies (%d non-transitive, %d transitive), %d predicates\n", nv, ntran + nnontran, nnontran, ntran, npred);
	
	n++;
	return true;
}

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

	command = strdup("rm -f ");
	command = catMem(command, TMP_FILE);
	if (system(command) == -1){
		printf("Error on removing %s.\n", TMP_FILE);
		return;
	}
	free(command);
    	command = strdup("rm -f ");
	command = catMem(command, SCRIPT_PATH);
	if (system(command) == -1){
		printf("Error on removing %s.\n", SCRIPT_PATH);
		return;
	}
	free(command);
	command = strdup("rm -f ");
	command = catMem(command, TRIVIAL_ENCODING_FILE);
	if (system(command) == -1){
		printf("Error on removing %s.\n", TRIVIAL_ENCODING_FILE);
		return;
	}
	free(command);
	command = strdup("rm -f ");
	command = catMem(command, CONSTRAINTS_FILE);
	if (system(command) == -1){
		printf("Error on removing %s.\n", CONSTRAINTS_FILE);
		return;
	}
	free(command);

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
#endif
	return 0;
}

int loadScenarios(char* file_in, FILE *fp){
	if (!alternative)
		puts("Using 'f = x + y * predicate' to deal with predicates.\n");
	else
		puts("Using 'f = x * (y + predicate)' to deal with predicates.\n");

	fp = freopen(file_in, "r", stdin);
	if (fp == NULL){
		fprintf(stderr, "Error opening %s\n", file_in);
		removeTempFiles();
		return -1;
	}

	n = 0;
	while(scanf("%s", s) == 1)
	{
		if (s[0] == '#')
		{
			if(fgets(s, sizeof(s), stdin) == NULL){
				fprintf(stderr,"Error reading scenario.\n");
				fclose(fp);
				removeTempFiles();
				return -1;
			}
			continue;
		}
		
		if (!strcmp(s, ".scenario"))
		{
			if(scanf("%s", s) == EOF){
				fprintf(stderr,"Error reading scenario.\n");
				fclose(fp);
				removeTempFiles();
				return -1;
			}
			printf("Loading scenario '%s'... ", s);
			scenarioNames.push_back(s);
			if (!readScenario()) {
				fprintf(stderr,"Error reading scenario.\n");
				fclose(fp);
				removeTempFiles();
				return -1;
			}
		}
		else
		{
			fprintf(stderr,"Wrong file format.\n");
			fclose(fp);
			removeTempFiles();
			return -1;
		}
	}

	return 0;

}

int predicateSearch(){

	bool predicates_found = false;

	for(int i = 0; i < V; i++)
	if (eventPredicates[i].size() > 0)
	{
		if (!predicates_found)
		{
			predicates_found = true;
			puts("\nList of predicates:");
		}
		printf("%s:", eventNames_str[i].c_str());
		map<string, int>::iterator p = eventPredicates[i].begin(), 
			q = eventPredicates[i].end();
		while(p != q)
		{
			string pr = p->first;
			printf(" %s", pr.c_str());
			p++;
		}
		puts("");
	}
	if (!predicates_found) puts("\nNo predicates found.");

	return 0;
}

int nonTrivialConstraints(FILE *fp, int *total, int *trivial){

	for(int i = 0; i < V; i++)
	{
		int np = eventPredicates[i].size();
		
		for(int j = 0; j <= np; j++) ev[i][j] = "";
		if (np == 0)
		{
			for(int j = 0; j < n; j++) if (g[j].v[i]) ev[i][0] += "1"; else ev[i][0] += "0";
			constraints[ev[i][0]].push_back(make_pair(-1, i));
		}
		else
		{
			if (!alternative)
			{	
				for(int j = 0; j < n; j++) if (g[j].v[i] && !g[j].pred[i]) ev[i][0] += "1"; else ev[i][0] += "0";
				constraints[ev[i][0]].push_back(make_pair(-1, i));
				for(int k = 1; k <= np; k++)
				{
					for(int j = 0; j < n; j++)
					if (!g[j].v[i]) ev[i][k] += "0";
					else
					{
						if (g[j].pred[i] == 0) ev[i][k] += "-";
						else
						if (g[j].pred[i] == k) ev[i][k] += "1";
						else
							ev[i][k] += "0";
					}
					constraints[ev[i][k]].push_back(make_pair(-k - 1, i));
				}
			}
			else
			{
				for(int j = 0; j < n; j++) if (g[j].v[i]) ev[i][0] += "1"; else ev[i][0] += "0";
				constraints[ev[i][0]].push_back(make_pair(-1, i));
				for(int k = 1; k <= np; k++)
				{
					for(int j = 0; j < n; j++)
					if (!g[j].v[i]) ev[i][k] += "-";
					else
					{
						if (g[j].pred[i] == 0) ev[i][k] += "1";
						else
						if (g[j].pred[i] == k) ev[i][k] += "0";
						else
							ev[i][k] += "1";
					}
					constraints[ev[i][k]].push_back(make_pair(-k - 1, i));
				}
			}
		}
	}
	
	for(int i = 0; i < V; i++)
	for(int j = 0; j < V; j++)
	if (i != j)
	{
		ee[i][j] = "";
		for(int k = 0; k < n; k++)
		{
			if (g[k].e[i][j] == 2 || ME(k, i, j)) ee[i][j] += "-";
			else
			if (g[k].e[i][j] == 1) ee[i][j] += "1";
			else ee[i][j] += "0";
		}
		constraints[ee[i][j]].push_back(make_pair(i, j));
	}
	
	cp = constraints.begin(); cq = constraints.end();
	while(cp != cq)
	{
		string s = cp->first;
		fprintf(fp,"%s       ", s.c_str());
		int k = cp->second.size();
		for(int i = 0; i < k; i++)
		{
			int a = cp->second[i].first;
			int b = cp->second[i].second;
			if (a < 0)
			{
				if (a == -1) fprintf(fp," %s", eventNames_str[b].c_str());
				else fprintf(fp," %s:%s", eventNames_str[b].c_str(), getPredicateName(b, -a - 1).c_str());
			}
			else
			{
				fprintf(fp," (%s -> %s)", eventNames_str[a].c_str(), eventNames_str[b].c_str());
			}
		}
		fprintf(fp,"\n");
		cp++;

		Encoding e;
		
		e.constraint = s;
		e.trivial = true;
		e.constant = 0;
		
		for(int i = 0; i < n; i++) if (s[i] == '1') { e.trivial = false; break;}

		if (!e.trivial)
		{
			e.trivial = true;
			e.constant = 1;
			
			for(int i = 0; i < n; i++) if (s[i] == '0') { e.trivial = false; break;}
		}
		
		encodings.push_back(e);
	}
	fclose(fp);
	*total = constraints.size();

	
	for(int i = 0; i < (*total); i++) if (encodings[i].trivial) (*trivial)++;

	return 0;
}

int conflictGraph(int *total){

	for(int i = 0; i < (*total); i++)
	if (!encodings[i].trivial)
	{
		string s = encodings[i].constraint;
		cgv.push_back(s);
		for(int j = 0; j < n; j++) if (s[j] == '0') s[j] = '1'; else if (s[j] == '1') s[j] = '0';
		cgv.push_back(s);
	}
	
	cge.resize(cgv.size());
	literal.resize(cgv.size());
	bestLiteral.resize(cgv.size());
	for(unsigned int i = 0; i < cgv.size(); i += 2) { bestLiteral[i] = i / 2; bestLiteral[i + 1] = -1;}
	
	for(unsigned int i = 0; i < cgv.size(); i++)
	for(unsigned int j = 0; j < cgv.size(); j++)
	{
		string a = cgv[i];
		string b = cgv[j];
		
		bool conflict = false;
		
		for(int k = 0; k < n; k++)
			if ((a[k] == '0' && b[k] == '1') || (a[k] == '1' && b[k] == '0'))
			{
				conflict = true;
				break;
			}
		
		if (conflict) cge[i].push_back(1); else cge[i].push_back(0);
	}
	

	return 0;
}

bool encode(int vertex, int limit, int next)
{
	if (next > limit) return false;
	
	if (vertex == ((int)cgv.size()) ) return true;
	
	for(int select = 0; select < 2; select++)
	{
		for(int i = 0; i <= next; i++)
		{
			int j;
			for(j = 0; j < vertex; j++)
			if (cge[vertex + select][j] && literal[j] == i) break;
			
			if (j == vertex)
			{
				literal[vertex + select] = i;
				if (encode(vertex + 2, limit, next + (i == next))) return true;
				literal[vertex + select] = -1;
			}
		}
	}
	return false;
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

/*READ ENCODING SET FUNCTION*/
/*This function reads the encoding set by designer in order to fix them.*/
int read_set_encoding(char *custom_file_name,int cpog_count, int *bits){
	FILE *fp = NULL;
	int i,k;
	char number[MAX_NAME];
	boolean acq = FALSE;

	fp = fopen(custom_file_name,"r");
	custom_perm = (int*) malloc(sizeof(int) * cpog_count);
	custom_perm_back = (int*) malloc(sizeof(int) * cpog_count);
	DC_custom = (boolean*) calloc(cpog_count,sizeof(boolean));

	for (i=0;i<cpog_count;i++){
		if( (fscanf(fp,"%s", number) == EOF)){
			fprintf(stderr,"Error: not enough encoding for this CPOG, they must be %d.\n",cpog_count);
			return 1;
		}
		if(number[0] != '/'){
			// RESET number of bits
			if(!acq){
				*bits = strlen(number);
				acq = TRUE;
				tot_enc = 1;
				for(k=0;k<(*bits);k++) tot_enc *= 2;
			}
			k = conv_int(number, i);
			custom_perm[i] = k;
			custom_perm_back[i] = k;
		}
		else{
			custom_perm[i] = -1;
			custom_perm_back[i] = -1;
		}
	}

	if (fscanf(fp,"%s", number) == EOF){
		fprintf(stderr,"Error on reading number of bits set inside file %s.\n", custom_file_name);
		return 2;
	}
	*bits = atoi(number);
	tot_enc = 1;
	for(k=0;k<(*bits);k++) tot_enc *= 2;
	fclose(fp);

	
	manual_file = (char**) malloc(sizeof(char*) * cpog_count);
	manual_file_back = (char**) malloc(sizeof(char*) * cpog_count);
	for(i=0;i<cpog_count;i++){
		manual_file[i] = (char*) malloc(sizeof(char) * ((*bits)+1));
		manual_file_back[i] = (char*) malloc(sizeof(char) * ((*bits)+1));
	}


	if( (fp = fopen(custom_file_name,"r")) == NULL ){
		fprintf(stderr,"Error on opening manual file.\n");
		return 2;
	}

	for(i=0;i<cpog_count;i++){
		if(fscanf(fp,"%s",manual_file[i]) == EOF){
			fprintf(stderr,"Error on reading custom encodings.\n");
			return 3;
		}
		strcpy(manual_file_back[i],manual_file[i]);
	}

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
					print_binary(NULL,k, bits);
					number = numb;
					if(!strDCcmp(number,manual_file[i],bits)){
						if(opcodes[k] == 1){
							ins = FALSE;
							if(it > 1000){
								print_binary(NULL,k, bits);
								number = numb;
								fprintf(stderr,"Op-code %s and %s cannot be set at the same time.\n", manual_file_back[i], number);
								return 2;
							}
						}
						opcodes[k] = 1;
					}
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

/*DIFFERENCE MATRIX FUNCTION*/
/*Following function computes differences among CPOG, and store them inside
a matrix.*/
int difference_matrix(int cpog_count, int len_sequence){
	int i = 0, j = 0, k = 0;
	
	/*COMPUTING DIFFERENCES AMONG CPOG*/
	for(i=0;i<cpog_count -1;i++)
		for(j=i+1;j<cpog_count;j++)
			for(k=0;k<len_sequence;k++)
				if((diff[k][i] == '0' && diff[k][j] == '1') || (diff[k][i] == '1' && diff[k][j] == '0'))
					opt_diff[i][j]++;
	
	/*OPTIMAL DIFFERENCES MATRIX PRINTING*/
	// debug printing
	/*if(verbose){
		printf("\nOPTIMAL DIFFERENCES MATRIX:\n");
		for(i=0;i<cpog_count;i++){
			for(j=0;j<cpog_count;j++)
				printf("%2d ", opt_diff[i][j]);
			printf("\n");
		}
		printf("\n");
	}*/
	return 0;
}
