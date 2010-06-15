## Takes in object and number of lines to be read, 0 means read to end.

read.orders <- function(ob, n = 0)

    ob = .update(ob)
    feed = ob@feed
    feed.index = ob@feed.index

    ob.data = ob@ob.data
    current.pos = ob@current.pos

    ids = ob@ids

    feed <- file(feed, open="r")


    x <- scan(feed, nline = 1, sep = ",", what = "character")



    while(length(line) != 0){
        if (line[1]=="A"){
            ob.data[current.pos,] = c(x[4], x[5], x[6], x[2], x[3])
            ids[x[3]] = current.pos
            current.pos = current.pos + 1
        }

        if (line[1]=="C"){
            ob.data[ids[x[3]],][1] = NA
        }

        if (line[1]=="R"){
            ob <- replace.order(ob, id = as.numeric(line[3]),
                                size = as.numeric(line[4]))
        }

        line <- readLines(filecon, n=1)
    }


    close.connection(filecon)

    invisible(ob)
}
