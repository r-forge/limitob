#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include "uthash.h"
#include "utlist.h"
#include <string.h>
#include <stdlib.h>
#include <stddef.h>

//this means that the longest order needs to be less than 100
//characters, probably true since there are a lot of characters

#define MAX_LEN 100

typedef struct trade{
    int row;
    char time[MAX_LEN];
    char price[MAX_LEN];
    char size[MAX_LEN];
    char mine[MAX_LEN];
    struct trade *next;
} trade;

trade *head = NULL;

SEXP getTrades(SEXP filename){
    /* defining stuff, f is the file that we open, str is what we put
       strings into delimiters is comma because its a csv file, token
       and strcp are where we copy str and other things into so that
       we don't have problems with strtok
     */

    FILE *f;
    SEXP retVector;
    char str[MAX_LEN], tmpstr[MAX_LEN];
    const char delimiters[] = ",";
    char *token, *strcp;
    int len, i = 0, j = 0, h = 1;
    trade *tmp, *t;


    f = fopen(CHAR(STRING_ELT(filename, 0)), "r");

    if(f == NULL)
	error("failed to open file");


    while(fgets(str, MAX_LEN, f) != NULL){
	strcp = strdup(str);

	token = strtok(strcp, delimiters);
	if(strcmp(token, "T") == 0){
	    t = (trade*)malloc(sizeof(trade));

	    t->row = h;

	    token = strtok(NULL, delimiters);
	    strcpy(t->time, token);

	    token = strtok(NULL, delimiters);
	    token = strtok(NULL, delimiters);
	    strcpy(t->price, token);

	    token = strtok(NULL, delimiters);
	    strcpy(t->size, token);

	    token = strtok(NULL, delimiters);
	    strcpy(t->mine, token);

	    LL_APPEND(head, t);

	    i++;
	}
	free(strcp);
	h++;
    }
    fclose(f);

    len = i * 5;
    PROTECT(retVector = allocVector(STRSXP, len));

    LL_FOREACH(head, t){

	sprintf(tmpstr, "%d", t->row);
	SET_STRING_ELT(retVector, j, mkChar(tmpstr));
	j++;

	SET_STRING_ELT(retVector, j, mkChar(t->time));
	j++;

	SET_STRING_ELT(retVector, j, mkChar(t->price));
	j++;

	SET_STRING_ELT(retVector, j, mkChar(t->size));
	j++;

	SET_STRING_ELT(retVector, j, mkChar(t->mine));
	j++;
    }
    LL_FOREACH_SAFE(head, t, tmp){
	LL_DELETE(head, t);
	free(t);
    }
    UNPROTECT(1);

    return retVector;

}
