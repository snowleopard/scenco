int getEventID(string name)
{
	if (eventNames.count(name) == 1) return eventNames[name];
	eventNames_str[V] = name;
	eventNames[name] = V++;
	return V - 1;
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
