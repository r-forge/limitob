library(kcm.orderbook)

load("sample.RData")

tt <- inside.market(ob)

stopifnot(
	isTRUE(all.equal(tt[1,1], 11.53))
) 

stopifnot(
	isTRUE(all.equal(tt[1,2], 100))
)
 
stopifnot(
	isTRUE(all.equal(tt[2,1], 11.35))
) 
 
stopifnot(
	isTRUE(all.equal(tt[2,2], 1000))
)  
stopifnot(
	isTRUE(tt[1,3]=="ASK")
)
stopifnot(
	isTRUE(tt[2,3]=="BID")
)