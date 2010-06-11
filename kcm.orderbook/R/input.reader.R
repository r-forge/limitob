read.orders <- function(ob, input){

    filecon <- file(input, open="r")
	pos <- seek(filecon, rw="r")

	.time  = 2
	.id	   = 3
	.price = 4
	.size  = 5
	.type  = 6

	e.o.f = FALSE 

	while(!e.o.f){
		tt <- readLines(filecon, n=1)
		if (!(e.o.f = (length(tt) < 2))){
		   tt <- unlist(strsplit(tt, ","))		   
		   
		   if (tt[1]=="A"){		   
		   	  ob <- add.order(ob, time  = as.numeric(tt[.time]), 
								  id	= as.numeric(tt[.id]),
			  	 				  price = as.numeric(tt[.price]),
								  size	= as.numeric(tt[.size]),
								  type 	= tt[.type])								  
		   }

		   if (tt[2]=="R"){
		   	  ob <- remove.order(ob, id = as.numeric(tt[.id]))			  			  
		   }

		   if (tt[3]=="C"){
		   	  ob <- replace.order(ob, id = as.numeric(tt[.id]),
			  	 					  size = as.numeric(tt[.size]))
		   }
		   
		   pos <- seek(filecon, rw="r")
		}
	}
	close.connection(filecon)

	return (ob)
}