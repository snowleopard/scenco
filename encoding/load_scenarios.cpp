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
			if (!g[n].v[vid] || g[n].pred[vid])
			{
				fprintf(stderr,"Incorrect predicate '%s'\n", to.c_str());
				return false;
			}
			from.erase(0, 1);
			g[n].pred[vid] = getPredicateID(vid, from);
			continue;
		}
		if (!check(from))
		{
			fprintf(stderr,"Cannot parse event '%s'\n", from.c_str());
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
				fprintf(stderr,"Cannot parse event '%s'\n", to.c_str());
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
		fprintf(stderr,"a cyclic event dependency detected!");
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
	
	fprintf(fpLOG,"%d events, %d dependencies (%d non-transitive, %d transitive), %d predicates\n", nv, ntran + nnontran, nnontran, ntran, npred);
	
	n++;
	return true;
}

int loadScenarios(char* file_in, FILE *fp){
	if (!alternative)
		fprintf(fpLOG,"Using 'f = x + y * predicate' to deal with predicates.\n");
	else
		fprintf(fpLOG,"Using 'f = x * (y + predicate)' to deal with predicates.\n");

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
			fprintf(fpLOG,"Loading scenario '%s'... ", s);
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
			fprintf(fpLOG,"\nList of predicates:");
		}
		fprintf(fpLOG,"%s:", eventNames_str[i].c_str());
		map<string, int>::iterator p = eventPredicates[i].begin(), 
			q = eventPredicates[i].end();
		while(p != q)
		{
			string pr = p->first;
			fprintf(fpLOG," %s", pr.c_str());
			p++;
		}
		//fprintf(fpLOG,"");
	}
	if (!predicates_found) fprintf(fpLOG,"\nNo predicates found.");

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

/*READ FILE FUNCTION*/
/*Following function read non-trivial encoding constraints of the Conditional Partial Order Graphs.*/
int read_file(char *file_in,int *cpog_count, int *len_sequence){
	FILE *fp = NULL;
	char string[MAX_CPOG], dump;
	int i = 0, j = 0;

	fp = fopen(file_in, "r");
	if( fscanf(fp,"%s", string) == EOF){
		fprintf(stderr,"File is empty. Please, introduce another file.\n");
		return -1;
	}
	*len_sequence = 1;
	(*cpog_count) = strlen(string);
	
	while(fscanf(fp,"%s", string) != EOF)
		(*len_sequence)++;
	fclose(fp);

	/*DEBUG PRINTING*/
	//printf("%d %d\n", *cpog_count, *len_sequence);

	diff = (char**) malloc(sizeof(char*) * (*len_sequence));
	for(i=0;i<(*len_sequence);i++)
		diff[i] = (char*) malloc(sizeof(char) * (*cpog_count));

	fp = fopen(file_in, "r");
	for(i = 0; i< (*len_sequence);i++){
		for(j = 0; j< (*cpog_count); j++){
			if(fscanf(fp, "%c", &diff[i][j]) == EOF){
				fprintf(stderr,"Error on reading custom encodings.\n");
				return 3;
			}
		}
		j = fscanf(fp,"%c", &dump);
	}
	fclose(fp);

	/*NON-TRIVIAL ENCODING PRINTING*/
	// debug printing
	/*if(verbose){		
		printf("NON-TRIVIAL ENCODING:\n");
		for(i = 0; i <(*len_sequence) ;i++){
			for(j = 0; j< (*cpog_count); j++)
				printf("%c", diff[i][j]);
			printf("\n");
		}
	}*/
	return 0;
}


