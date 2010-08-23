#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include "uthash.h"
#include <string.h>
#include <stdlib.h>
#include <stddef.h>

//this means that the longest order needs to be less than 100
//characters, probably true since there are a lot of characters

#define MAX_LEN 100

struct order{
    char id[MAX_LEN];
    char type[MAX_LEN];
    char status[MAX_LEN];
    long time;
    double price;
    int size;
    UT_hash_handle hh;
};

struct order *orderbook_multiple = NULL;


SEXP readOrdersMultiple(SEXP filename, SEXP Rrows){
    /* defining stuff, f is the file that we open, str is what we put
       strings into delimiters is comma because its a csv file, token
       and strcp are where we copy str and other things into so that
       we don't have problems with strtok
     */
    struct order *s, *t;
    FILE *f;
    char str[MAX_LEN];
    const char delimiters[] = ",";
    char *token, *strcp, *ptr;
    int *rows;
    int i = 1, j, n, k, len, size;
    SEXP retList, retVector;

    rows = INTEGER(Rrows);
    n = length(Rrows);

    //Allocate return list, trade vector, and cancel vector

    PROTECT(retList = allocVector(VECSXP, n));

    // open the file
    f = fopen(CHAR(STRING_ELT(filename, 0)), "r");

    // if the file can't be opened
    if(f == NULL)
	error("failed to open file");


    //now do it for the other things in row.
    for(j = 0; j < n; j++){
	while(i <= rows[j] && fgets(str, MAX_LEN, f) != NULL){
	    strcp = strdup(str);

	    token = strtok(strcp, delimiters);

	    if(strcmp(token, "A") == 0){

		s = malloc(sizeof(struct order));

		token = strtok(NULL, delimiters);
		s->time = atol(token);

		token = strtok(NULL, delimiters);
		strcpy(s->id, token);

		HASH_FIND_STR(orderbook_multiple, token, t);

		token = strtok(NULL, delimiters);
		s->price = atof(token);

		token = strtok(NULL, delimiters);
		s->size = atoi(token);

		token = strtok(NULL, delimiters);
		strcpy(s->type, token);

		token = strtok(NULL, delimiters);

		if((ptr = strchr(token, '\n')) != NULL)
		    *ptr = '\0';

		strcpy(s->status, token);

		/*special feature--if you add something with the same
		  ID, it will replace the order. that way we don't need to worry about
		  having a "replace" message--we can just have add messages with
		  different order size*/

		if(t == NULL){
		    HASH_ADD_STR(orderbook_multiple, id, s);
		} else{
		    t->size = s->size;
		    free(s);
		}


	    }else if(strcmp(token, "C") == 0){

		token = strtok(NULL, delimiters);
		token = strtok(NULL, delimiters);

		// Get rid of the new line character

		if((ptr = strchr(token, '\n')) != NULL)
		    *ptr = '\0';

		HASH_FIND_STR(orderbook_multiple, token, s);

		if(s){
		    HASH_DEL(orderbook_multiple, s);
		    free(s);
		}

	    }else if(strcmp(token, "R") == 0){

		token = strtok(NULL, delimiters);
		token = strtok(NULL, delimiters);

		HASH_FIND_STR(orderbook_multiple, token, s);

		token = strtok(NULL, delimiters);
		size = atoi(token);

		if(s)
		    s->size = size;


	    }

	    free(strcp);
	    i++;
	}

	k = 0;
	len = HASH_COUNT(orderbook_multiple) * 6;

	PROTECT(retVector = allocVector(STRSXP, len));

	for(s = orderbook_multiple; s != NULL; s = s->hh.next){

	    sprintf(str, "%ld", s->time);
	    SET_STRING_ELT(retVector, k, mkChar(str));
	    k++;

	    SET_STRING_ELT(retVector, k, mkChar(s->id));
	    k++;

	    SET_STRING_ELT(retVector, k, mkChar(s->type));
	    k++;

	    sprintf(str, "%f", s->price);
	    SET_STRING_ELT(retVector, k, mkChar(str));
	    k++;

	    sprintf(str, "%d", s->size);
	    SET_STRING_ELT(retVector, k, mkChar(str));
	    k++;

	    SET_STRING_ELT(retVector, k, mkChar(s->status));
	    k++;

	}

	SET_VECTOR_ELT(retList, j, retVector);
	UNPROTECT(1);
    }

    fclose(f);

    while(orderbook_multiple) {
	s = orderbook_multiple;          /* copy pointer to first item     */
	HASH_DEL(orderbook_multiple, s);  /* delete; users advances to next */
	free(s);            /* optional- if you want to free  */
    }

    UNPROTECT(1);
    return retList;


}
