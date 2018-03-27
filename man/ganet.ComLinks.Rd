\name{ganet.ComLinks}
\alias{ganet.ComLinks}

\title{Common Links finder from two list of interactions}
\description{
It takes two arguments as input. First one is the list of interactions that the function will find its common interactions with the  second list of iterations that is entered as the second argument to the function. 
}
\usage{
ganet.ComLinks(netlist, netdata)
}
\arguments{
  \item{netlist}{
It is a matrix of which first and second columns have the interactions for each gene pair. The rest is just ignored. It must have unique interactions.
}
  \item{netdata}{
It is a matrix of which first and second columns have the interactions for each gene pair. The rest is just ignored. It must have unique interactions.
}
}
\details{
This may be used for instance to search for whether the predicted gene/protein interactions are overlapping in a biological interaction database.
}
\value{
It results three columns matrix. The first and the second columns have the overlapping interactions. The last one has the indices showing the overlapping positions in the first argument.
}
\references{
  1. G. Altay, N. Altay, D.E. Neal, Global assessment of network inference algorithms based on available literature of gene/protein interactions, Turk J Biol, 37:547-555, 2013.
  2. G. Altay, J Zapardel, B Peters, "Determining preprocessing combinations and sample size for Gene Network Inference over ...", bioArxiv, 2017.
}
\author{
Gokmen Altay
}
\note{
This package is intended to be updated with respect to feedbacks and also with updates of the current databases.
}

\seealso{
\code{\link{ganet.FEtest}},   \code{\link{ganet.combine}}, \code{\link{ganet.UniqNetSimp}}
}
\examples{
#data(ganet.ex.net)  # assume this is the predicted protein interactions
#data(hprd)          # This is the HPRD database to search for in it.

#Validated <- ganet.ComLinks(netlist= as.matrix(ganet.ex.net), 
#netdata=as.matrix(hprd))

}

% R documentation directory.
\keyword{overlap}

