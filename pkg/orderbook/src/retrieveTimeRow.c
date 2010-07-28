#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdio.h>
#include "uthash.h"
#include <string.h>
#include <stdlib.h>
#include <stddef.h>

//this means that the longest order needs to be less than 100
//characters, probably true since there are a lot of characters

#define MAX_LEN 100


void retrieveTimeRow(char** filename, int *a, int *b){
    /* defining stuff, f is the file that we open, str is what we put
       strings into delimiters is comma because its a csv file, token
       and strcp are where we copy str and other things into so that
       we don't have problems with strtok
     */

    FILE *f;
    char str[MAX_LEN];
    const char delimiters[] = ",";
    char *token, *strcp;
    int i = 1;

    f = fopen(filename[0], "r");

    if(f == NULL)
	error("failed to open file");

    while(fgets(str, MAX_LEN, f) != NULL){
	strcp = strdup(str);

	token = strtok(strcp, delimiters);
	token = strtok(NULL, delimiters);

	if(atoi(token) >= *a){
	    *b = i;
	    break;
	}
	free(strcp);
	i++;
    }

    fclose(f);
}

static R_NativePrimitiveArgType retrieve[3] = {STRSXP, INTSXP, INTSXP};

static const R_CMethodDef cMethods[] = {
    {"retrieveTimeRow", (DL_FUNC) &retrieveTimeRow, 3, retrieve},
    {NULL, NULL, 0}
};

void R_init_retrieveTimeRow(DllInfo *info){

    R_registerRoutines(info,cMethods, NULL, NULL, NULL);
}
