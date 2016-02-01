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
