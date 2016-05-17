bool transitiveClosure(int index)
{
	for(int i = 0; i < eventsLimit; i++)
	if (g[index].v[i])
		for(int j = 0; j < eventsLimit; j++)
		if (g[index].v[j] && g[index].e[j][i])
			for(int k = 0; k < eventsLimit; k++)
			if (g[index].v[k] && g[index].e[i][k]) g[index].e[j][k] = 1;

	for(int i = 0; i < eventsLimit; i++) if (g[index].e[i][i]) return false;

	for(int i = 0; i < eventsLimit; i++)
	if (g[index].v[i])
		for(int j = 0; j < eventsLimit; j++)
		if (g[index].v[j] && g[index].e[j][i])
			for(int k = 0; k < eventsLimit; k++)
			if (g[index].v[k] && g[index].e[i][k]) g[index].e[j][k] = 2;

	return true;
}

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

void resizeGraphStructure(int size){

	g = (GRAPH_TYPE*) realloc(g, sizeof(GRAPH_TYPE) * size);
	g[n-1].e = new int*[eventsLimit];
	for(int j = 0; j<eventsLimit; j++){
		g[n-1].e[j] = new int[eventsLimit];
		for(int k=0; k<eventsLimit;k++){
			g[n-1].e[j][k] = 0;
		}
	}
	g[n-1].v = new int[eventsLimit];
	g[n-1].pred = new int[eventsLimit];
	for(int k=0; k<eventsLimit;k++){
		g[n-1].v[k] = 0;
		g[n-1].pred[k] = 0;
	}
}

bool readScenario()
{
	resizeGraphStructure(++n);

	while(1)
	{
		if(fgets(s, sizeof(s), stdin) == NULL){
			fprintf(stderr,"error on reading the file\n");
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
				fprintf(stderr,"Cannot parse predicate '%s'\n", s);
				return false;
			}
			int vid = getEventID(to);
			if (!g[n-1].v[vid] || g[n-1].pred[vid])
			{
				fprintf(stderr,"Incorrect predicate '%s'\n", to.c_str());
				return false;
			}
			from.erase(0, 1);
			g[n-1].pred[vid] = getPredicateID(vid, from);
			continue;
		}
		if (!check(from))
		{
			fprintf(stderr,"Cannot parse event '%s'\n", from.c_str());
			return false;
		}
		if (!(str >> to))
		{
			g[n-1].v[getEventID(from)] = 1;
			continue;
		}
		int fromID = getEventID(from), toID;
		
		g[n-1].v[fromID] = 1;
		
		do
		{
			if (!check(to))
			{
				fprintf(stderr,"Cannot parse event '%s'\n", to.c_str());
				return false;
			}
			
			toID = getEventID(to);
			g[n-1].v[toID] = 1;
			g[n-1].e[fromID][toID] = 1;
			
			fromID = toID;
		} while(str >> to);
	}
	
	if (!transitiveClosure(n-1))
	{
		fprintf(stderr,"a cyclic event dependency detected!");
		return false;
	}
	
	int nv = 0, ntran = 0, nnontran = 0, npred = 0;
	
	for(int i = 0; i < eventsLimit; i++)
	{
		if (g[n-1].v[i]) nv++;
		if (g[n-1].pred[i]) npred++;
	}
	for(int i = 0; i < eventsLimit; i++)
	for(int j = 0; j < eventsLimit; j++)
	{
		if (g[n-1].e[i][j] == 1) nnontran++;
		if (g[n-1].e[i][j] == 2) ntran++;
	}
	
	//printf("%d events, %d dependencies (%d non-transitive, %d transitive), %d predicates\n", nv, ntran + nnontran, nnontran, ntran, npred);
	
	return true;
}

int loadScenarios(char* file_in, FILE *fp){
	/*if (!alternative)
		fprintf(fpLOG,"Using 'f = x + y * predicate' to deal with predicates.\n");
	else
		fprintf(fpLOG,"Using 'f = x * (y + predicate)' to deal with predicates.\n");*/

	fp = freopen(file_in, "r", stdin);
	if (fp == NULL){
		fprintf(stderr, "Error opening %s\n", file_in);
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
				return -1;
			}
			continue;
		}
		
		if (!strcmp(s, ".scenario"))
		{
			if(scanf("%s", s) == EOF){
				fprintf(stderr,"Error reading scenario.\n");
				fclose(fp);
				return -1;
			}
			//printf("Loading scenario '%s'... ", s);
			scenarioNames.push_back(s);
			if (!readScenario()) {
				fprintf(stderr,"Error reading scenario.\n");
				fclose(fp);
				return -1;
			}
		}
		else
		{
			fprintf(stderr,"Wrong file format.\n");
			fclose(fp);
			return -1;
		}
	}
	fclose(fp);
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
			//printf("\nList of predicates:");
		}
		//printf("%s:", eventNames_str[i].c_str());
		map<string, int>::iterator p = eventPredicates[i].begin(), 
			q = eventPredicates[i].end();
		while(p != q)
		{
			string pr = p->first;
			//printf(" %s", pr.c_str());
			p++;
		}
		//fprintf(fpLOG,"");
	}
	//if (!predicates_found) printf("\nNo predicates found.");

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

/*DIFFERENCE MATRIX FUNCTION*/
/*Following function computes differences among CPOG, and store them inside
a matrix.*/
int difference_matrix(int cpog_count){
	int i = 0, j = 0, k = 0;
	
	/*COMPUTING DIFFERENCES AMONG CPOG*/
	for(i=0;i<cpog_count -1;i++)
		for(j=i+1;j<cpog_count;j++)
			for(k=0;k<len_sequence;k++)
				if((diff[k][i] == '0' && diff[k][j] == '1') || (diff[k][i] == '1' && diff[k][j] == '0'))
					opt_diff[i][j]++;
	
	return 0;
}

/*READ FILE FUNCTION*/
/*Following function read non-trivial encoding constraints of the Conditional Partial Order Graphs.*/
int read_file(char *file_in){
	FILE *fp = NULL;
	char *string, dump, c;
	int i = 0, j = 0;

	fp = fopen(file_in, "r");
	if(feof(fp)){
		fprintf(stderr,"File is empty. Please, introduce another file.\n");
		return -1;
	}
	while ( (c = fgetc (fp)) != '\n' ) i++;
	i++;
	fclose(fp);
	string = (char*) malloc(sizeof(char) * (i+1));

	fp = fopen(file_in, "r");
	if(fscanf(fp,"%s", string) == EOF){
		fprintf(stderr,"Error reading the file.\n");
		return -1;
	}
	len_sequence = 1;
	cpog_count = strlen(string);
	
	while(fscanf(fp,"%s", string) != EOF)
		len_sequence++;
	fclose(fp);

	diff = (char**) malloc(sizeof(char*) * (len_sequence));
	for(i=0;i<len_sequence;i++)
		diff[i] = (char*) malloc(sizeof(char) * cpog_count);

	fp = fopen(file_in, "r");
	for(i = 0; i< len_sequence;i++){
		for(j = 0; j< cpog_count; j++){
			if(fscanf(fp, "%c", &diff[i][j]) == EOF){
				fprintf(stderr,"Error on reading custom encodings.\n");
				return -1;
			}
		}
		j = fscanf(fp,"%c", &dump);
	}
	fclose(fp);

	return 0;
}

/*READ_CONSTRAINTS FUNCTION*/
/*Following function read encoding constraints of every vertex and edge of CPOG*/
int read_cons(char *file_cons){
	FILE *fp = NULL;
	char *name, c;
	int i = 0, j = 0, v_tmp = 0, p = 0;
	boolean ins; 

	if( (fp = fopen(file_cons, "r")) == NULL){
		printf("Error on opening constraints file.\n");
		return 1;

	}
	if( feof(fp) ){
		printf("File is empty. Please, introduce another file.\n");
		return 1;
	}
	if(fclose(fp) != 0){
		printf("Error on closing constraints file.\n");
		return 1;
	}

	vertices = NULL;	

	fp = fopen(file_cons, "r");
	while( !feof(fp) ){
		for(i = 0 ; i < cpog_count+8; i++) c = fgetc(fp);
		c = fgetc(fp);
		int h = 0;
		while( c != '\n' && c != EOF){
			if (h != 0) c = fgetc(fp);
			h++;

			switch(c){
				case ' ':
					/*Parsing space characters*/
					break;
				case '(':
					/*Parsing edges*/
					while( (c = fgetc(fp)) != ')');
					c = fgetc(fp);
					break;
				default:
					/*Parsing vertices name*/
					name = strdup("");
					while( (!isspace(c)) && c != ':' && c != EOF){
						name = catChar(name, c);
						c = fgetc(fp);
					}
					
					if(c == ':')
						while( c != ' ' && c != EOF){
					/*Taking into account nested conditions on vertices*/
							if(c == '('){
								p++;
								c = fgetc(fp);
								while(p){
									while(c != ')'){
										if(c== '(') p++;
										c = fgetc(fp);
									}
									p--;
									c = fgetc(fp);
								}
							}else
								c = fgetc(fp);
						}
					ins = TRUE;
					for(j=0;j<v_tmp;j++)
						if(strcmp(name, vertices[j]) == 0)
							ins = FALSE;
					if(ins){
						num_vert++;
						v_tmp++;
						vertices = (char**) realloc(vertices,sizeof(char*) * v_tmp);
						vertices[v_tmp-1] = strdup(name);
					}
					while((!isspace(c)) && c != EOF)
						c = fgetc(fp);
					free(name);
					break;
			}
		}
	}
	fclose(fp);
	return 0;
}

/*ACQUIRE CPOG FUNCTION*/
/*Following function read again constraints file in order to definetly acquire CPOG.*/
void parsing_cpog(char* file_cons){
	FILE *fp = NULL;
	char *name = NULL, c;
	char *source = NULL, *dest = NULL, *truth = NULL, *cond = NULL;
	int i = 0, j = 0,k=0, n_tmp = 0, p = 0;
	boolean ins, condit;

	truth = (char*) malloc(sizeof(char) * (cpog_count + 2));
	fp = fopen(file_cons, "r");
	while(  !feof(fp) ){
		if(fscanf(fp,"%s",truth) == 0){
			printf("Error on reading the truth table inside constraints file.\n");
			return;
		}
		
		/*DEBUG PRINTING: reading truth table*/
		//printf("%s\n", truth);

		
		for(i = 0 ; i < 8; i++) c = fgetc(fp);
		c = fgetc(fp);
		int h = 0;
		while( c != '\n' && c != EOF){
			if (h != 0) c = fgetc(fp);
			h++;

			switch(c){
				case ' ':
					/*Parsing space characters*/
					break;
				case '(':
					/*PARSING EDGES*/
					c = fgetc(fp);
					/*PARSING SOURCE VERTEX*/
					source = strdup("");
					while((!isspace(c))){

						source = catChar(source, c);
						c = fgetc(fp);
					}

					while( (c = fgetc(fp)) != '>');
					c = fgetc(fp);
					c = fgetc(fp);
					
					/*PARSING DESTINATION VERTEX*/
					dest = strdup("");
					while(c != ')'){
						dest = catChar(dest, c);
						c = fgetc(fp);
					}

					c = fgetc(fp);

					/*EDGE INFORMATION INSERTION*/
					ins = FALSE;
					for(j=0;j<num_vert && !ins;j++){
						fflush(stdout);
						if(strcmp(source, cpog[j][0].source) == 0)
							for(k=0;k<num_vert && !ins;k++){
								if(strcmp(dest, cpog[j][k].dest) == 0){
									ins = TRUE;
									cpog[j][k].truth = strdup(truth);
									cpog[j][k].cond = NULL;
									cpog[j][k].truth_cond = NULL;
								}
							}
					}
					break;
				default:
					/*PARSING VERTICES*/
					
					/*Parsing vertex name*/
					name = strdup("");
					while( (!isspace(c)) && c != ':' && c != EOF){
						name = catChar(name, c);
						c = fgetc(fp);
					}
					
					/*Parsing condition of vertex*/
					condit = FALSE;
					if(c == ':'){
						condit = TRUE;
						c = fgetc(fp);
						cond = strdup("");
						while( (!isspace(c)) && c != EOF){
							/*Taking into account nested conditions on vertices*/
							if(c == '('){
								p++;
								cond = catChar(cond, c);
								c = fgetc(fp);
								while(p){
									while(c != ')'){
										if(c == '(') p++;
										cond = catChar(cond, c);
										c = fgetc(fp);
									}
									p--;
									cond = catChar(cond, c);
									c = fgetc(fp);
								}
							}
							else{
								cond = catChar(cond, c);
								c = fgetc(fp);
							}
						}
					}

					/*VERTEX INFORMATION INSERTION*/
					ins = FALSE;
					for(j=0;j<num_vert && !ins;j++)
						if(strcmp(name, cpog[j][0].source) == 0){
							ins = TRUE;
							if (condit){
								cpog[j][j].condition = TRUE;
								cpog[j][j].cond = strdup(cond);
								cpog[j][j].truth_cond = strdup(truth);
							}
							else
								cpog[j][j].truth = strdup(truth);
						}
					break;
					if(source != NULL){
						free(source);
						source = NULL;
					}
					if(dest != NULL){
						free(dest);
						dest = NULL;
					}
					if(cond != NULL){
						free(cond);
						cond = NULL;
					}
					if(name != NULL){
						free(name);
						name = NULL;
					}
			}
		}
	}
	fclose(fp);

	return;
}

/*GET CONDITIONS NAMES FUNCTION*/
/*Following function parses CPOG representation in order to get names of the conditions present
in same vertices.*/
int get_conditions_names(){
	int i,k,j,p;
	boolean ins = TRUE;
	char *name;
	
	for(i=0;i<nv; i++){
		if(cpog[i][i].condition){
			k = strlen(cpog[i][i].cond);
			j = 0;
			while(j<k){
				name = strdup("");
				switch(cpog[i][i].cond[j]){
					case '(':
						j++;
						break;
					case ' ':
						j++;
						break;
					case '!':
						j++;
						break;
					case ')':
						j++;
						break;
					case '+':
						j++;
						break;
					case '*':
						j++;
						break;
					case '\0':
						j = k;
						break;
					default:
						while( (!isspace(cpog[i][i].cond[j])) &&
							cpog[i][i].cond[j] != '\0' && 
							cpog[i][i].cond[j] != ')' && 
							cpog[i][i].cond[j] != '+' && 
							cpog[i][i].cond[j] != '*'){
								name = catChar(name,cpog[i][i].cond[j++]);
						}
						ins = TRUE;
						for(p=0;p<n_cond;p++)
							if(!strcmp(name_cond[p],name))
								ins = FALSE;
						if (ins){
							n_cond++;
							name_cond = (char**) realloc(name_cond,sizeof(char*) * n_cond);
							name_cond[n_cond-1] = strdup(name);
						}
						break;
				}
				free(name);
			}
		}
	}
	return 0;
}

int readingGraphStructure(){

	int err;

	if( (err = read_cons(CONSTRAINTS_FILE)) ){
		fprintf(stderr,"Error occured while reading constraints\
			file, error code: %d", err);
		return -1;
	}

	/*CPOG ALLOCATION*/
	cpog = (CPOG_TYPE**) malloc(sizeof(CPOG_TYPE*) * (num_vert));
	if(cpog == NULL){
		fprintf(stderr,"Allocation of CPOG memory failed.\n");
		return -1;
	}
	for(int i=0;i<num_vert; i++){
		cpog[i] = (CPOG_TYPE*) malloc(sizeof(CPOG_TYPE) * (num_vert));
		if(cpog[i] == NULL){
			fprintf(stderr,"Allocation of CPOG memory (%d)\
				failed.\n", i);
			return -1;
		}
		for(int j = 0; j<num_vert;j++){
			cpog[i][j].source = NULL;
			cpog[i][j].dest = NULL;
			cpog[i][j].cond = NULL;
			cpog[i][j].truth = NULL;
			cpog[i][j].truth_cond = NULL;
			cpog[i][j].fun = NULL;
			cpog[i][j].fun_cond = NULL;
		}
	}

	nv = num_vert; /*Due to overwriting problem*/

	/*CPOG DEFINITION*/
	for(int i=0;i<num_vert; i++)
		for(int j=0;j<num_vert;j++)
			if(i == j){
				cpog[i][j].type = 'v';
				cpog[i][j].source = strdup(vertices[i]);
				cpog[i][j].dest = strdup("X");
				cpog[i][j].condition = FALSE;
			}
			else{
				cpog[i][j].type = 'e';
				cpog[i][j].source = strdup(vertices[i]);
				cpog[i][j].dest = strdup(vertices[j]);
				cpog[i][j].condition = FALSE;
			}

	/*SECOND READING OF ENCODING FILE*/
	parsing_cpog(CONSTRAINTS_FILE);

	return 0;
}

void computeCodesAvailable(){
	tot_enc = 1;
	for(int k=0;k<bits;k++) tot_enc *= 2;
	return;
}
