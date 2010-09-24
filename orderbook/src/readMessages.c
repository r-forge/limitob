#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <glib.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>

/* compiler commands 

   gcc -I/usr/share/R/include
   -I/usr/lib/glib-2.0/include -I/usr/include/glib-2.0 -fpic
   -std=gnu99 -O3 -pipe -g -c readMessages.c -o readMessages.o

   gcc -shared -o readMessages.so readMessages.o -L/usr/lib64/R/lib
   -lR -lglib-2.0 */


//this means that the longest order needs to be less than 100
//characters, which is true

#define MAX_LEN 20

//its comma delimited
#define DELIMITERS ","

/*declaring our hash tables, ob is the hash table that stores
  everything, smallob just stores everything between the last time we
  put everything into the R structure */

//this is our order structure
struct order
{
    char id[MAX_LEN];
    char type[MAX_LEN];
    char status[MAX_LEN];
    char time[MAX_LEN];
    char price[MAX_LEN];
    char size[MAX_LEN];
};


//add order function, returns 1 if an order from the main order book was replaced, 0 otherwise
GSList *addOrder(char *str, GHashTable *ob, GHashTable *smallob, GSList *cancels)
{

  char *token, *ptr;
  struct order *s, *t;
  
  //allocate memory for order
  s = g_malloc(sizeof(struct order));
  
  //"0th" token is the order type
  token = strtok(str, DELIMITERS);
  
  //first token is time
  token = strtok(NULL, DELIMITERS);
  strcpy(s->time, token);
  
  //second token is id
  token = strtok(NULL, DELIMITERS);
  strcpy(s->id, token);

  //third token is price
  token = strtok(NULL, DELIMITERS);
  strcpy(s->price, token);

  //fourth token is size
  token = strtok(NULL, DELIMITERS);
  strcpy(s->size, token);
  
  //fifth token is type
  token = strtok(NULL, DELIMITERS);
  strcpy(s->type, token);

  //sixth token is status
  token = strtok(NULL, DELIMITERS);
  
  // trim the newline
  if((ptr = strchr(token, '\n')) != NULL)
    *ptr = '\0';
  
  strcpy(s->status, token);
  
  // See if this ID is already in the main order book
  
  t = (struct order *)g_hash_table_lookup(ob, s->id);
  //if its in the order book this "add" was actually a "replace"
  if(t){
    // so replace the size
    strcpy(t->size, s->size);
    
    //add the pointer to the old order to the current hash
    g_hash_table_insert(smallob, t->id, t);
    
    //add to cancels
    cancels = g_slist_prepend(cancels, g_strdup(s->id));
    
    //free this order
    free(s);
    
    return cancels;
  }
  
  //if its not in the main order book check to see if its in the small order book
  
  t = (struct order *)g_hash_table_lookup(smallob, s->id);
  
  //if its in the small order book then its a replace for one of those orders
  if(t){
    
    // replace the size and free
    strcpy(t->size, s->size);
    free(s);
    
    //if it wasn't in either order book, add it to both
  } else {
    
    g_hash_table_insert(ob, s->id, s);
    g_hash_table_insert(smallob, s->id, s);
    
  }
  
  return cancels;
}

//cancel order
GSList *cancelOrder(char *str, GHashTable *ob, GHashTable *smallob, GSList *cancels)
{
  
  char *token, *ptr;
  
  //"0th" token is the order type
  token = strtok(str, DELIMITERS);

  //time token
  token = strtok(NULL, DELIMITERS);
  
  //id token
  token = strtok(NULL, DELIMITERS);
  
  // Get rid of the new line character
  
  if((ptr = strchr(token, '\n')) != NULL)
    *ptr = '\0';
  
  //remove from the small order book, auto frees value
  if(g_hash_table_steal(smallob, token)){
    g_hash_table_remove(ob, token);
  } else {
    
    //append to cancel list
    cancels = g_slist_prepend(cancels, g_strdup(token));

  }

  return cancels;
}

//replace order
GSList *replaceOrder(char *str, GHashTable *ob, GHashTable *smallob, GSList *cancels)
{

  char *token, *ptr, *id;
  struct order *s;
  
  //"0th" token is the order type
  token = strtok(str, DELIMITERS);
  
  //token is time
  token = strtok(NULL, DELIMITERS);

  //token is id
  id = strtok(NULL, DELIMITERS);
  
  //find ID in small hash
  s = (struct order *)g_hash_table_lookup(smallob, id);
  
  //token is size
  token = strtok(NULL, DELIMITERS);
  
  // Get rid of the new line character
  if((ptr = strchr(token, '\n')) != NULL)
    *ptr = '\0';
  
  //if its in the small order book
  if(s){
    //replace size and return
    strcpy(s->size, token);
    
    return cancels;
    
  } 
  
  //if its not in the small one, then find it in the big one
  s = (struct order *)g_hash_table_lookup(ob, id);
  
  if(s){
    //replace size
    strcpy(s->size, token);
    
    //add to small hash
    g_hash_table_insert(smallob, id, s);
    
    //append to cancel list
    cancels = g_slist_prepend(cancels, g_strdup(id));

  }
  
  return cancels;
}

//iterate through all active orders and create a matrix

SEXP iterateOrders(GHashTable *hash)
{
  GHashTableIter iter;
  char *key;
  struct order *value;
  int i = 0, j, ncol, nrow;
  SEXP tempob, dim;

  nrow = g_hash_table_size(hash);
  ncol = 6;

  //allocate vector of characters (strings)
  PROTECT(tempob = allocVector(STRSXP, nrow * ncol));

  //create an iterator

  g_hash_table_iter_init(&iter, hash);

  while(g_hash_table_iter_next(&iter, (gpointer)&key, (gpointer)&value)){

    j = 0;
    
    //set time, then ID, type, price, size, status
    SET_STRING_ELT(tempob, i + nrow*j++, mkChar(value->time));
    
    SET_STRING_ELT(tempob, i + nrow*j++, mkChar(value->id));
    
    SET_STRING_ELT(tempob, i + nrow*j++, mkChar(value->type));
      
    SET_STRING_ELT(tempob, i + nrow*j++, mkChar(value->price));
    
    SET_STRING_ELT(tempob, i + nrow*j++, mkChar(value->size));
    
    SET_STRING_ELT(tempob, i + nrow*j++, mkChar(value->status));
    
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

SEXP iterateCancels(GSList *cancels)
{
  GSList *iterator = NULL;
  SEXP cancelVec;
  int i;
  
  PROTECT(cancelVec = allocVector(STRSXP, g_slist_length(cancels)));

  for(i = 0, iterator = cancels; iterator; ++i, iterator = iterator->next)
    SET_STRING_ELT(cancelVec, i, mkChar(iterator->data));
  
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
    int i = 1, j, n;
    SEXP retList, obList, cancelList;
    GHashTable *ob = g_hash_table_new_full(g_str_hash, g_str_equal, NULL, g_free);
    GHashTable *smallob = g_hash_table_new_full(g_str_hash, g_str_equal, NULL, g_free);
    GSList *cancels = NULL;

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
      
      strcpy(strcp, str);
      token = strtok(strcp, DELIMITERS);
      
      if(strcmp(token, "A") == 0){
	addOrder(str, ob, smallob, cancels);
      } else if(strcmp(token, "C") == 0)
	cancelOrder(str, ob, smallob, cancels);
      else if(strcmp(token, "R") == 0)
	replaceOrder(str, ob, smallob, cancels);
      
      ++i;
    }

    //put this into the obList
    SET_VECTOR_ELT(obList, 0, iterateOrders(ob));

    for(j = 1; j < n; j++){
      
      g_slist_free(cancels);
      cancels = NULL;
      g_hash_table_steal_all(smallob);
      
      while(i <= rows[j] && fgets(str, 100, f) != NULL){

	strcpy(strcp, str);

	token = strtok(strcp, DELIMITERS);
	
	if(strcmp(token, "A") == 0)
	  cancels = addOrder(str, ob, smallob, cancels);
	else if(strcmp(token, "C") == 0)
	  cancels = cancelOrder(str, ob, smallob, cancels);
	else if(strcmp(token, "R") == 0)
	  cancels = replaceOrder(str, ob, smallob, cancels);
	
	
	++i;
      }
      
      SET_VECTOR_ELT(obList, j, iterateOrders(smallob));
      SET_VECTOR_ELT(cancelList, j, iterateCancels(cancels));
      
    }

    fclose(f);

    g_hash_table_steal_all(smallob);
    g_hash_table_steal_all(ob);

    SET_VECTOR_ELT(retList, 0, obList);
    SET_VECTOR_ELT(retList, 1, cancelList);

    UNPROTECT(3);

    return retList;
}

	 

