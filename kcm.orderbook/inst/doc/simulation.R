simulate <- function(ob, n=100000, 
                     action.prob = c(cancel=0.5, market=0.2, limit=0.30),
                     order.type  = 0.5,
                     alpha = 0.3,
                     in.spread.prob = 0.35,
                     ...){
   
  #might need to do something first if the orderbook is empty
    
    x = ob@current.ob
    ob.names = ob@ob.names
    tmp.midpoint = mid.point(ob)
    tmp.bestask = best.ask(ob)
    tmp.bestbid = best.bid(ob)
    
    

    for(i in 1:n) {
            
            x = ob@current.ob
          if(mid.point(ob) == 0){
                current.price = tmp.midpoint
          } else { 
                current.price <- mid.point(ob)
          }
          
          isbuy = runif(1) < order.type
            
          #action.prob[1] = min(action.prob[1], action.prob[1] * signif(total.orders(ob)/100, 2))
          if(total.orders(ob) < 250){
                action.prob[1] <- 0
                action.prob[4] = 1 - sum(action.prob[1:3])
          } else {
                action.prob[1] = 0.5
                action.prob[4] = 1 - sum(action.prob[1:3])
          }
          
        
          action <- sample(c("Cancel", "Market", "Limit", ""), size=1, prob=action.prob)
          
          if (action == "Cancel") {
            #//pick an existing ID and cancel the order
            
                ob <- remove.order(ob, sample(x[[ob.names[5]]], size = 1))
            
          }
          else if (action == "Market") {
            
            #//set a new price/ or tick
            if(isbuy) {                          
                ob <- market.order(ob, type="BUY", size = best.ask(ob)[2] )
            } else {
                ob <- market.order(ob, type="SELL", size = best.bid(ob)[2] )
            }
            
          }                              
          if (action == "Limit") {
           
            if(spread(ob) <= 0.01){
                    spread.diff = 0
                } else {
                    spread.diff = round(runif(1, 0, spread(ob)), 2)
                }
            
            out.diff = round((mid.point(ob)*.1)*runif(1)^1/(1 + alpha), 2)
            
            
            
            in.spread = runif(1) < in.spread.prob
            size = round(exp(rnorm(1, mean = 4.5, sd = .8)))
            
            if(isbuy & in.spread){
          
                ob <- add.order(ob, price= max(0, best.bid(ob)[1] + spread.diff), size, type="BID")
            } else if(isbuy & !in.spread){
                ob <- add.order(ob, price = max(0, best.bid(ob)[1] - out.diff), size, type = "BID")                
            } else if(!isbuy & in.spread){

               ob <- add.order(ob, price= max(0, best.ask(ob)[1] - spread.diff), size, type="ASK")
            } else if (!isbuy & !in.spread){                                 
                ob <- add.order(ob, price = max(0, best.ask(ob)[1] + out.diff), size, type = "ASK")
            } 
            
            
            
          }
    }
    
    
     invisible(ob)
} 