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

int sequentialEncoding(){

	return 0;
}

int satBasedEncoding(){

	return 0;
}

int randomEncoding(){

	return 0;
}

int heuristicEncoding(){

	return 0;
}

int exhaustiveEncoding(){

	return 0;
}
