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
    int id;
    long time;
    char type[MAX_LEN];
    double price;
    int size;
    char trade[MAX_LEN];
    UT_hash_handle hh;
};

struct order *orderbook = NULL;


void add_order(int newid, long newtime, char* newtype, double newprice, int newsize, char* newtrade){
    struct order *s;

    s = malloc(sizeof(struct order));
    s->id = newid;
    s->time = newtime;
    strcpy(s->type, newtype);
    s->price = newprice;
    s->size = newsize;
    strcpy(s->trade, newtrade);
    HASH_ADD_INT(orderbook, id, s);
}

struct order *find_order(int key){
    struct order *s;
    HASH_FIND_INT(orderbook, &key, s);
    return s;
}
void cancel_order(struct order *s){
    HASH_DEL(orderbook, s);
    free(s);
}


void replace_order(struct order *s, int newsize){
	s->size = newsize;
}

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

	sprintf(tmp, "%d", s->id);
	SET_STRING_ELT(retVector, i, mkChar(tmp));
	i++;

	SET_STRING_ELT(retVector, i, mkChar(s->type));
	i++;

	sprintf(tmp, "%f", s->price);
	SET_STRING_ELT(retVector, i, mkChar(tmp));
	i++;

	sprintf(tmp, "%d", s->size);
	SET_STRING_ELT(retVector, i, mkChar(tmp));
	i++;

	SET_STRING_ELT(retVector, i, mkChar(s->trade));
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
    struct order *s;
    FILE *f;
    char str[MAX_LEN], type[MAX_LEN], trade[MAX_LEN];
    const char delimiters[] = ",";
    char *token, *strcp;
    int i = 1, id, size;
    long time;
    double price;


    f = fopen(CHAR(STRING_ELT(filename, 0)), "r");

    if(f == NULL)
	error("failed to open file");


    while(fgets(str, MAX_LEN, f) != NULL && i <= INTEGER(msgs)[0]){
	strcp = strdup(str);

	token = strtok(strcp, delimiters);

	if(strcmp(token, "A") == 0){

	    token = strtok(NULL, delimiters);
	    time = atol(token);

	    token = strtok(NULL, delimiters);
	    id = atoi(token);

	    token = strtok(NULL, delimiters);
	    price = atof(token);

	    token = strtok(NULL, delimiters);
	    size = atoi(token);

	    token = strtok(NULL, delimiters);
	    strcpy(type, token);

	    token = strtok(NULL, delimiters);
	    strcpy(trade, token);


	    add_order(id, time, type, price, size, trade);
	} else if(strcmp(token, "C") == 0){
	    token = strtok(NULL, delimiters);
	    token = strtok(NULL, delimiters);

	    id = atoi(token);
	    s = find_order(id);
	    cancel_order(s);


	} else if(strcmp(token, "R") == 0){

	    token = strtok(NULL, delimiters);
	    token = strtok(NULL, delimiters);
	    id = atoi(token);

	    token = strtok(NULL, delimiters);
	    size = atoi(token);

	    s = find_order(id);
	    replace_order(s, size);

	}
	/*
	if(strcmp(token, "T") == 0){
	    while(token != NULL){
		if(strcmp(token, "TRUE\n") == 0){
		    trades[j] = i;
		    j++;
		}
		token = strtok(NULL, delimiters);
	    }
	}
	*/
	free(strcp);
	i++;
    }

    fclose(f);

    return iterate_orders();
}
