\name{o.and.l}
\alias{o.and.l}

\title{
(O)rders and (l)evels associated with an orderbook.
}
\description{
Calculate orderbook data associated with (o)rders and (l)evels.
}
\usage{
o.and.l(x, side, type)
}

\arguments{
  \item{x}{
An object of class orderbook.
}
  \item{side}{
Must be ASK, BID or ALL.
}
  \item{type}{
Orders or levels.
}
}
\details{
Simple function to replace several unnecessary methods.
}
\value{
 Numeric.
}
\author{
David Kane
}


\seealso{
\code{\link{orderbook}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, side, type){

    ## Query an orderbook object and find out key data from it.

    stopifnot(class(x) == "orderbook")
    stopifnot(side \%in\% c("BID", "ASK"))
    stopifnot(type \%in\% c("levels", "orders"))



  }
}

\keyword{misc}

