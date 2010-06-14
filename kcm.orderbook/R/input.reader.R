read.orders <- function(ob, input){

    filecon <- file(input, open="r")
    pos <- seek(filecon, rw="r")

    line <- readLines(filecon, n=1)



    while(length(line) != 0){
        if (line[1]=="A"){
            ob <- add.order(ob, time  = as.numeric(line[2]),
                            id	= as.numeric(line[3]),
                            price = as.numeric(line[4]),
                            size	= as.numeric(line[5]),
                            type 	= line[6])
        }

        if (line[1]=="C"){
            ob <- remove.order(ob, id = as.numeric(line[3]))
        }

        if (line[1]=="R"){
            ob <- replace.order(ob, id = as.numeric(line[3]),
                                size = as.numeric(line[4]))
        }

        pos <- seek(filecon, rw="r")
        line <- readLines(filecon, n=1)
    }


    close.connection(filecon)

    invisible(ob)
}
