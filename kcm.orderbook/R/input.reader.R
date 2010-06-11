read.orders <- function(ob, input){

    filecon <- file(input, open="r")
	pos <- seek(filecon, rw="r")

	.time  = 1
	.id	   = 2
	.price = 3
	.size  = 4
	.type  = 5

	e.o.f = FALSE 

	while(!e.o.f){
		tt <- readLines(filecon, n=1)
		if (!(e.o.f = (length(tt) < 2))){
		   tt <- unlist(strsplit(tt, ","))		   
		   
		   if (length(tt)==5){		   
		   	  ob <- add.order(ob, time  = as.numeric(tt[.time]), 
								  id	= as.numeric(tt[.id]),
			  	 				  price = as.numeric(tt[.price]),
								  size	= as.numeric(tt[.size]),
								  type 	= tt[.type])								  
		   }

		   if (length(tt)==2){
		   	  ob <- remove.order(ob, id = as.numeric(tt[.id]))			  			  
		   }

		   if (length(tt)==3){
		   	  ob <- replace.order(ob, id = as.numeric(tt[.id]),
			  	 					  size = as.numeric(tt[.size]))
		   }
		   
		   pos <- seek(filecon, rw="r")
		}
	}
	close.connection(filecon)

	return (ob)
}