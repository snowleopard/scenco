char* readString(FILE *fIn, int *status){

	char c, *str;
	int cnt = 0;

	*status = 0;

	// consumes spaces
	while( (c = fgetc(fIn)) == ' ');

	// read chars
	str = (char*) malloc(sizeof(char )* 0);
	while(c != ' ' && c != '\n' && c != EOF){
		str = (char *) realloc(str, sizeof(char) * (++cnt));
		str[cnt-1] = c;
		c = fgetc(fIn);
	}

	if(c == '\n') *status = 1;
	if(c == EOF) *status = 2;

	// append terminator char
	str = (char *) realloc(str, ++cnt);
	str[cnt-1] = '\0';

	return str;
}

int convertGraph(FILE *fIn, FILE *fOut){

	char *name, *source, *dest;
	char *syntax;

	int status = 0;

	// name
	name = readString(fIn, &status);
	if(status != 0) return status;

	syntax = readString(fIn, &status);
	if(strcmp(syntax, "=") != 0){
		fprintf(stderr,"Syntax error, '=' not present.");
		return -1;
	}
	free(syntax);

	fprintf(fOut,".scenario %s\n", name);
	free(name);

	// first arc
	source = readString(fIn, &status);
	syntax = readString(fIn, &status);
	if(strcmp(syntax, "->") != 0){
		fprintf(stderr,"Syntax error, '->' not present.");
		return -1;
	}
	free(syntax);
	dest = readString(fIn, &status);
	fprintf(fOut,"%s %s\n", source, dest);
	free(source);
	free(dest);
	
	while(status == 0){
		syntax = readString(fIn, &status);
		if(strcmp(syntax, "+") != 0){
			fprintf(stderr,"Syntax error, '+' not present.");
			return -1;
		}
		free(syntax);

		source = readString(fIn, &status);

		syntax = readString(fIn, &status);
		if(strcmp(syntax, "->") != 0){
			fprintf(stderr,"Syntax error, '->' not present.");
			return -1;
		}
		free(syntax);

		dest = readString(fIn, &status);
		fprintf(fOut,"%s %s\n", source, dest);
		free(source);
		free(dest);
	}

	fprintf(fOut, ".end\n\n");

	return status;
}

int selectMode(char *fileName){

	FILE *fp;
	int status = 0;
	int isLog = 0;
	char *firstWord;

	fp = fopen(fileName, "r");
		firstWord = readString(fp, &status);
		if(strcmp(firstWord, ".scenario") != 0) isLog = 1;
		free(firstWord);
	fclose(fp);
	
	return isLog;
}

int parseScenarios(char *fileName){

	FILE *fIn, *fOut;
	int stat= 0;
	int isLog = 0;
	char c;

	isLog = selectMode(fileName);

	fIn = fopen(fileName, "r");
	fOut = fopen(SCENARIOS, "w");

	if (isLog){
		while(1){
			stat = convertGraph(fIn, fOut);
			if(stat == -1) break;
			if(stat == 2) break;
		}
	} else {
		while( (c = fgetc(fIn)) != EOF ) fprintf(fOut, "%c", c);
	}

	fclose(fIn);
	fclose(fOut);

	if(stat == -1){
		return -1;
	}
	
	return 0;
}
