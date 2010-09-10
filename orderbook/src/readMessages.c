#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include "uthash.h"
#include "utlist.h"
#include <string.h>
#include <stdlib.h>
#include <stddef.h>

//this means that the longest order needs to be less than 100
//characters, which is true

#define MAX_LEN 20

//its comma delimited
#define DELIMITERS ","

typedef struct cancel{
    char id[MAX_LEN];
    struct cancel *next;
} cancel;

struct order
{
    char id[MAX_LEN];
    char type[MAX_LEN];
    char status[MAX_LEN];
    char time[MAX_LEN];
    char price[MAX_LEN];
    char size[MAX_LEN];
    UT_hash_handle hh1, hh2;
};

//this is used for the initial build
struct order *ob = NULL;

//this is used for holding later orders
struct order *smallob = NULL;

//list for holding the cancels
cancel *head = NULL;

//add order function
int addOrder(char *str, int i)
{
    char *token, *ptr;
    cancel *r;
    struct order *s, *t;
    Rprintf("%s\n", str);
    //allocate memory for order
    s = malloc(sizeof(struct order));
    
    //"0th" token is the order type
    token = strtok(str, DELIMITERS);
    Rprintf("%s,", token);


    //first token is time
    token = strtok(NULL, DELIMITERS);
    Rprintf("%s,", token);
    strcpy(s->time, token);

    //second token is id
    token = strtok(NULL, DELIMITERS);
    Rprintf("%s,", token);
    strcpy(s->id, token);

    //third token is price
    token = strtok(NULL, DELIMITERS);
    Rprintf("%s,", token);
    strcpy(s->price, token);

    //fourth token is size
    token = strtok(NULL, DELIMITERS);
    Rprintf("%s,", token);
    strcpy(s->size, token);

    //fifth token is type
    token = strtok(NULL, DELIMITERS);
    Rprintf("%s,", token);
    strcpy(s->type, token);

    //sixth token is status
    token = strtok(NULL, DELIMITERS);

    // trim the newline
    if((ptr = strchr(token, '\n')) != NULL)
	*ptr = '\0';

    strcpy(s->status, token);
    Rprintf("%s", token);

    /*special feature--if you add something with the same ID, it will
      replace the order. that way we don't need to worry about having
      a "replace" message--we can just have add messages with
      different order size. if the order is from the original order
      book (ob), then delete * the order and read it with the same
      timestamp. return the ID * of the order */

    Rprintf("finding in ob\n");

    //here we check to see if ID is already in the order book
    HASH_FIND(hh1, ob, s->id, strlen(s->id), t);

    if(t){
        //we want to keep the original timestamp
	strcpy(s->time, t->time);

	//add the order current hash
	HASH_ADD(hh2, smallob, id, strlen(s->id), s);

	r = (cancel*)malloc(sizeof(cancel));

	strcpy(r->id, s->id);

	//add ID as a cancel ID
	LL_APPEND(head, r);
	Rprintf("found");

	return ++i;
    }

    Rprintf("finding in smallob\n");


    //check to see if its in the other order book
    HASH_FIND(hh2, smallob, s->id, strlen(s->id), t);

    if(t){

	/* if this order is just in the messages we are working with
	   replace the size and be done with it*/
      
      strcpy(t->size, s->size);
      free(s);

      Rprintf("found");

    } else {

	/* if it wasn't found then add it to both hashes */
	HASH_ADD(hh1, ob, id, strlen(s->id), s);
	HASH_ADD(hh2, smallob, id, strlen(s->id), s);
    }

    return 0;
}

//cancel order
int cancelOrder(char *str, int i)
{
    char *token, *ptr;
    cancel *r;
    struct order *s;

    //"0th" token is the order type
    token = strtok(str, DELIMITERS);
    Rprintf("%s,", token);

    //time token
    token = strtok(NULL, DELIMITERS);

    //id token
    token = strtok(NULL, DELIMITERS);

    // Get rid of the new line character

    if((ptr = strchr(token, '\n')) != NULL)
	*ptr = '\0';

    //find the ID
    HASH_FIND(hh2, smallob, token, strlen(token), s);

    if(s){
	HASH_DELETE(hh2, smallob, s);

	HASH_FIND(hh1, ob, token, strlen(token), s);
	HASH_DELETE(hh1, ob, s);

	free(s);
	return 0;
    }

    HASH_FIND(hh1, ob, token, strlen(token), s);

    if(s){
	r = (cancel*)malloc(sizeof(cancel));
	strcpy(r->id, s->id);

	HASH_DELETE(hh1, ob, s);
	free(s);

	LL_APPEND(head, r);
	return ++i;
    }
    
    return 0;
}

//replace order

int replaceOrder(char *str, int i)
{
    char *token, *ptr;
    cancel *r;
    struct order *s, *t;

    //"0th" token is the order type
    token = strtok(str, DELIMITERS);
    Rprintf("%s,", token);

    //token is time
    token = strtok(NULL, DELIMITERS);

    //token is id
    token = strtok(NULL, DELIMITERS);

    //find ID in  both  hashes
    HASH_FIND(hh1, ob, token,  strlen(token), s);
    HASH_FIND(hh2, smallob, token,  strlen(token), t);

    //token is size
    token = strtok(NULL, DELIMITERS);

    // Get rid of the new line character
    if((ptr = strchr(token, '\n')) != NULL)
	*ptr = '\0';

    //if its in the main  hash
    if(s){

	//replace size
	strcpy(s->size, token);

	//add it to the small one, correct size
	HASH_ADD(hh2, smallob, id, strlen(s->id), s);

	r = (cancel*)malloc(sizeof(cancel));
	strcpy(r->id, s->id);

	//return the ID
	LL_APPEND(head, r);
	return ++i;

    } else if(t){

	//replace  size
	strcpy(t->size, token);

	r = (cancel*)malloc(sizeof(cancel));
	strcpy(r->id, t->id);

	//return ID
	LL_APPEND(head, r);
	return ++i;
    }


    return 0;
}

//iterate through all active orders and create a matrix

SEXP iterateOrdersOB(void)
{
    struct order *s;
    int i = 0, j, ncol, nrow;
    SEXP tempob, dim;

    nrow = HASH_CNT(hh1, ob);
    ncol = 6;

    //allocate vector of characters (strings)
    PROTECT(tempob = allocVector(STRSXP, nrow * ncol));

    for(s = ob; s != NULL; s = s->hh1.next){
      Rprintf("%s,%s,%s,%s,%s,%s\n", s->time, s->id, s->type, s->price, s->size, s->status);

      j = 0;
      
      //set time, then ID, type, price, size, status
      SET_STRING_ELT(tempob, i + nrow*j++, mkChar(s->time));
      
      SET_STRING_ELT(tempob, i + nrow*j++, mkChar(s->id));
      
      SET_STRING_ELT(tempob, i + nrow*j++, mkChar(s->type));
      
      SET_STRING_ELT(tempob, i + nrow*j++, mkChar(s->price));
      
      SET_STRING_ELT(tempob, i + nrow*j++, mkChar(s->size));
      
      SET_STRING_ELT(tempob, i + nrow*j++, mkChar(s->status));
      
      ++i;
    }

    //setting the dimensions
    PROTECT(dim = allocVector(INTSXP, 2));
    INTEGER(dim)[0] = nrow;
    INTEGER(dim)[1] = ncol;

    //turning it into a matrix
    setAttrib(tempob, R_DimSymbol, dim);
    
    UNPROTECT(2);

    return tempob;

}

SEXP iterateOrdersSmall(void)
{
    struct order *s;
    int i = 0, j, ncol, nrow;
    SEXP tempob, dim;

    nrow = HASH_CNT(hh2, smallob);
    ncol = 6;

    Rprintf("%d\n", nrow);

    //allocate vector of characters (strings)
    PROTECT(tempob = allocVector(STRSXP, nrow * ncol));

    for(s = smallob; s != NULL; s = s->hh2.next){
      j = 0;
      Rprintf("%s,%s,%s,%s,%s,%s\n", s->time, s->id, s->type, s->price, s->size, s->status);

      //set time, then ID, type, price, size, status
      SET_STRING_ELT(tempob, i + nrow*j++, mkChar(s->time));
      
      SET_STRING_ELT(tempob, i + nrow*j++, mkChar(s->id));
      
      SET_STRING_ELT(tempob, i + nrow*j++, mkChar(s->type));
      
      SET_STRING_ELT(tempob, i + nrow*j++, mkChar(s->price));
      
      SET_STRING_ELT(tempob, i + nrow*j++, mkChar(s->size));
      
      SET_STRING_ELT(tempob, i + nrow*j++, mkChar(s->status));
      
      ++i;
    }
    
    //setting the dimensions
    PROTECT(dim = allocVector(INTSXP, 2));
    INTEGER(dim)[0] = nrow;
    INTEGER(dim)[1] = ncol;

    //turning it into a matrix
    setAttrib(tempob, R_DimSymbol, dim);
    
    UNPROTECT(2);

    return tempob;

}

//delete everything in the hash


void clearHash(void)
{
    struct order *s;

    while(ob){
	s = ob;
	HASH_DELETE(hh1, ob, s);
	free(s);
    }
    
}

SEXP iterateCancels(int len)
{
    cancel *r, *tmp;
    SEXP cancelVec;
    int i = 0;
    
    PROTECT(cancelVec = allocVector(STRSXP, len));

    LL_FOREACH_SAFE(head, r, tmp){
	SET_STRING_ELT(cancelVec, i, mkChar(r->id));
	++i;
	
	LL_DELETE(head, r);
	free(r);
    }
    UNPROTECT(1);
    return cancelVec;
}

	

SEXP readMessages(SEXP filename, SEXP Rrows)
{
    /* defining stuff, f is the file that we open, str is what we put
       strings into delimiters is comma because its a csv file, token
       and strcp are where we copy str and other things into so that
       we don't have problems with strtok
     */

    FILE *f;
    char str[100];
    char *token, strcp[100];
    int *rows;
    int i = 1, j, n, len = 0;
    SEXP retList, obList, cancelList;

    rows = INTEGER(Rrows);
    n = length(Rrows);

    //Allocate return list

    PROTECT(retList = allocVector(VECSXP, 2));
    PROTECT(obList = allocVector(VECSXP, n));
    PROTECT(cancelList = allocVector(VECSXP, n));

    // open the file
    f = fopen(CHAR(STRING_ELT(filename, 0)), "r");

    // if the file can't be opened
    if(f == NULL)
	error("failed to open file");


    //Build the initial order book until rows[0]

    while(i <= rows[0] && fgets(str, 100, f) != NULL){
      
      Rprintf("%d\n", i);
      strcpy(strcp, str);
      token = strtok(strcp, DELIMITERS);
      
      if(strcmp(token, "A") == 0){
	Rprintf("%c\n", 'A');
	addOrder(str, 0);
      }else if(strcmp(token, "C") == 0){
	cancelOrder(str, 0);
	Rprintf("%c\n", 'C');

      }else if(strcmp(token, "R") == 0){
	replaceOrder(str, 0);
	Rprintf("%c\n", 'R');
      }
      
      ++i;
    }

    //put this into the obList
    SET_VECTOR_ELT(obList, 0, iterateOrdersOB());

    for(j = 1; j < n; j++){

      HASH_CLEAR(hh2, smallob);

      Rprintf("%d\n", j);


      while(i <= rows[j] && fgets(str, 100, f) != NULL){

	Rprintf("%d\n", i);

	strcpy(strcp, str);

	token = strtok(strcp, DELIMITERS);
	
	if(strcmp(token, "A") == 0){
	  Rprintf("%c\n", 'A');

	  len = addOrder(str, len);
	}else if(strcmp(token, "C") == 0){
	  Rprintf("%c\n", 'C');

	  len = cancelOrder(str, len);
	} else if(strcmp(token, "R") == 0){
	  Rprintf("%c\n", 'T');
	  len = replaceOrder(str, len);
	}
	
	++i;
      }
      
      SET_VECTOR_ELT(obList, j, iterateOrdersSmall());
      SET_VECTOR_ELT(cancelList, j, iterateCancels(len));
      
    }

    fclose(f);

    clearHash();

    SET_VECTOR_ELT(retList, 0, obList);
    SET_VECTOR_ELT(retList, 1, cancelList);

    UNPROTECT(3);

    return retList;
}

	 

