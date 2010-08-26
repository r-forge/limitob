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

struct order *orderbook = NULL;



void delete_all() {
  struct order *s;

  while(orderbook) {
    s = orderbook;          /* copy pointer to first item     */
    HASH_DEL(orderbook, s);  /* delete; users advances to next */
    free(s);            /* optional- if you want to free  */
  }
}

SEXP iterate_orders(){
    struct order *s;
    SEXP retVector;
    char tmp[MAX_LEN];
    int i = 0, len = HASH_COUNT(orderbook);
    len = len * 6;

    PROTECT(retVector = allocVector(STRSXP, len));

    for(s = orderbook; s != NULL; s = s->hh.next){

	sprintf(tmp, "%ld", s->time);
	SET_STRING_ELT(retVector, i, mkChar(tmp));
	i++;

	SET_STRING_ELT(retVector, i, mkChar(s->id));
	i++;

	SET_STRING_ELT(retVector, i, mkChar(s->type));
	i++;

	sprintf(tmp, "%f", s->price);
	SET_STRING_ELT(retVector, i, mkChar(tmp));
	i++;

	sprintf(tmp, "%d", s->size);
	SET_STRING_ELT(retVector, i, mkChar(tmp));
	i++;

	SET_STRING_ELT(retVector, i, mkChar(s->status));
	i++;

    }

    delete_all();

    UNPROTECT(1);
    return retVector;

}


SEXP readOrders(SEXP filename, SEXP msgs){
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
    int i = 1, size;

    f = fopen(CHAR(STRING_ELT(filename, 0)), "r");

    if(f == NULL)
	error("failed to open file");


    while(fgets(str, MAX_LEN, f) != NULL && i <= INTEGER(msgs)[0]){
	strcp = strdup(str);

	token = strtok(strcp, delimiters);

	if(strcmp(token, "A") == 0){

	    s = malloc(sizeof(struct order));

	    token = strtok(NULL, delimiters);
	    s->time = atol(token);

	    token = strtok(NULL, delimiters);
	    strcpy(s->id, token);

	    HASH_FIND_STR(orderbook, token, t);

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

	    if(t == NULL){
		HASH_ADD_STR(orderbook, id, s);
	    } else{
		t->size = s->size;
		free(s);
	    }



	} else if(strcmp(token, "C") == 0){
	    token = strtok(NULL, delimiters);
	    token = strtok(NULL, delimiters);

	    // Get rid of the new line character

	    if((ptr = strchr(token, '\n')) != NULL)
		*ptr = '\0';

	    HASH_FIND_STR(orderbook, token, s);

	    if(s)
		HASH_DEL(orderbook, s);

	    free(s);

	} else if(strcmp(token, "R") == 0){

	    token = strtok(NULL, delimiters);
	    token = strtok(NULL, delimiters);

	    HASH_FIND_STR(orderbook, token, s);

	    token = strtok(NULL, delimiters);
	    size = atoi(token);

	    if(s)
		s->size = size;
	}

	free(strcp);
	i++;
    }

    fclose(f);

    return iterate_orders();
}
