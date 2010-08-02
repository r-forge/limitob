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


SEXP retrieveTimeRow(SEXP filename, SEXP Rtimes){
    /* defining stuff, f is the file that we open, str is what we put
       strings into delimiters is comma because its a csv file, token
       and strcp are where we copy str and other things into so that
       we don't have problems with strtok
     */

    FILE *f;
    char str[MAX_LEN];
    const char delimiters[] = ",";
    char *token, *strcp;
    int j = 0, i = 1, n;
    int *times;
    SEXP retVector;

    times = INTEGER(Rtimes);
    n = length(Rtimes);

    PROTECT(retVector = allocVector(INTSXP, n));

    f = fopen(CHAR(STRING_ELT(filename, 0)), "r");

    if(f == NULL)
	error("failed to open file");

    while(j < n && fgets(str, MAX_LEN, f) != NULL){
	strcp = strdup(str);

	token = strtok(strcp, delimiters);
	token = strtok(NULL, delimiters);

	if(atoi(token) >= times[j]){
	    INTEGER(retVector)[j] = i;
	    j++;
	}
	free(strcp);
	i++;
    }

    fclose(f);

    UNPROTECT(1);

    return(retVector);

}
