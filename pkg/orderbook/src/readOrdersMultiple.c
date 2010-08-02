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
    char trade[MAX_LEN];
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
    struct order *s;
    FILE *f;
    char str[MAX_LEN], tempid[MAX_LEN];
    const char delimiters[] = ",";
    char *token, *strcp, *ptr;
    int *rows;
    int i = 1, j, n, k, len, size, c = 0, t = 0;
    SEXP retList, retVector, tradeVec, cancelVec;

    rows = INTEGER(Rrows);
    n = length(Rrows);

    //Allocate return list, trade vector, and cancel vector

    PROTECT(retList = allocVector(VECSXP, n + 2));
    PROTECT(tradeVec = allocVector(STRSXP, rows[n -1] - rows[0]));
    PROTECT(cancelVec = allocVector(STRSXP, rows[n -1] - rows[0]));

    // open the file
    f = fopen(CHAR(STRING_ELT(filename, 0)), "r");

    // if the file can't be opened
    if(f == NULL)
	error("failed to open file");


    // read in to the first number of row number
    while(i <= rows[0] && fgets(str, MAX_LEN, f) != NULL){
	    strcp = strdup(str);

	    token = strtok(strcp, delimiters);


	    //if its add create the order structure and add to hash
	    if(strcmp(token, "A") == 0){

		s = malloc(sizeof(struct order));

		token = strtok(NULL, delimiters);
		s->time = atol(token);

		token = strtok(NULL, delimiters);
		strcpy(s->id, token);

		token = strtok(NULL, delimiters);
		s->price = atof(token);

		token = strtok(NULL, delimiters);
		s->size = atoi(token);

		token = strtok(NULL, delimiters);
		strcpy(s->type, token);

		token = strtok(NULL, delimiters);
		strcpy(s->trade, token);

		HASH_ADD_STR(orderbook_multiple, id, s);


		//if its a cancel then remove it
	    }else if(strcmp(token, "C") == 0){

		token = strtok(NULL, delimiters);
		token = strtok(NULL, delimiters);

		// Get rid of the new line character

		if((ptr = strchr(token, '\n')) != NULL)
		    *ptr = '\0';

		HASH_FIND_STR(orderbook_multiple, token, s);

		if(s)
		    HASH_DEL(orderbook_multiple, s);
		else
		    Rprintf("%d\n", i);

		free(s);

//if its a replace then replace the size. in the future might make this decrement.

	    } else if(strcmp(token, "R") == 0){

		token = strtok(NULL, delimiters);
		token = strtok(NULL, delimiters);

		HASH_FIND_STR(orderbook_multiple, token, s);

		token = strtok(NULL, delimiters);
		size = atoi(token);

		s->size = size;

	    }

	    free(strcp);
	    i++;
	}

	k = 0;
	len = HASH_COUNT(orderbook_multiple) * 6;

	PROTECT(retVector = allocVector(STRSXP, len));
	//insert the hash into a vector
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

	    SET_STRING_ELT(retVector, k, mkChar(s->trade));
	    k++;

	}

	SET_VECTOR_ELT(retList, 0, retVector);
	UNPROTECT(1);

	//now do it for the other things in row.
    for(j = 1; j < n; j++){
	while(i <= rows[j] && fgets(str, MAX_LEN, f) != NULL){
	    strcp = strdup(str);

	    token = strtok(strcp, delimiters);

	    if(strcmp(token, "A") == 0){

		s = malloc(sizeof(struct order));

		token = strtok(NULL, delimiters);
		s->time = atol(token);

		token = strtok(NULL, delimiters);
		strcpy(s->id, token);

		token = strtok(NULL, delimiters);
		s->price = atof(token);

		token = strtok(NULL, delimiters);
		s->size = atoi(token);

		token = strtok(NULL, delimiters);
		strcpy(s->type, token);

		token = strtok(NULL, delimiters);
		strcpy(s->trade, token);

		HASH_ADD_STR(orderbook_multiple, id, s);

	    }else if(strcmp(token, "C") == 0){

		token = strtok(NULL, delimiters);
		token = strtok(NULL, delimiters);

		// Get rid of the new line character

		if((ptr = strchr(token, '\n')) != NULL)
		    *ptr = '\0';

		strcpy(tempid, token);

		HASH_FIND_STR(orderbook_multiple, token, s);

		if(s){
		    SET_STRING_ELT(cancelVec, c, mkChar(token));
		    c++;

		    HASH_DEL(orderbook_multiple, s);
		    free(s);
		} else
		    Rprintf("%d\n", i);

	    } else if(strcmp(token, "T") == 0){

		SET_STRING_ELT(tradeVec, t, mkChar(tempid));
		t++;
		continue;

	    } else if(strcmp(token, "R") == 0){

		token = strtok(NULL, delimiters);
		token = strtok(NULL, delimiters);

		strcpy(tempid, token);
		HASH_FIND_STR(orderbook_multiple, token, s);

		token = strtok(NULL, delimiters);
		size = atoi(token);

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

	    SET_STRING_ELT(retVector, k, mkChar(s->trade));
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

    SET_VECTOR_ELT(retList, n, cancelVec);
    SET_VECTOR_ELT(retList, n + 1, tradeVec);

    UNPROTECT(3);
    return retList;


}
