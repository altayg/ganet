\name{ganet.FEtest}
\alias{ganet.FEtest}

\title{Fisher's Exact Test Overlapping Significance}
\description{
Determines a statistical p-value that shows the significance of the overlap between two networks. 
}
\usage{
ganet.FEtest(nOverlap, nPredicted, nFocusedSet, nUniverse)
}

\arguments{
  \item{nOverlap}{
The number of unique overlap.
}
  \item{nPredicted}{
The number of predcitions. 
}
\item{nFocusedSet}{
The number of elements (interactions) in the database of interest, regarding the genes of raw dataset. 
}
\item{nUniverse}{
The number of biologically possible (expected) interactions regarding the genes of raw dataset . 
}
}
\details{
This may be used for instance to search for whether the number of predicted gene/protein interactions (nPredicted) are overlapping in a biological interaction database of interest with a significant p-value.
}
\value{
It results an environment 'res' with two output: res$stats and res$precision. 
 \item{stats }{All the statistical results from Fisher's exact test. }
 \item{precision}{The rate of accurate predictions over all the predicted links.}

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
\code{\link{ganet.ComLinks}},   \code{\link{ganet.combine}}, \code{\link{ganet.UniqNetSimp}}
}
\examples{
#data(ganet.ex.net)  # assume this is the predicted protein interactions
#data(hprd)          # This is the HPRD database to search for in it.

#Validated <- ganet.ComLinks(netlist=as.matrix(ganet.ex.net), netdata=as.matrix(hprd))

#data(ganet.ex.genes)  # load the example unique genes of the example
#row dataset to compute the number of universe
#nUniverse <- length(ganet.ex.genes)*(length(ganet.ex.genes) - 1)/2  #
#all possible interactions with these genes. 
##This is the upper-bound and over-optimistic. 
#We recommend to take no more than half of it.
#nUniverse <- nUniverse/2

#nFocusedSet <- length( intersect(  which(hprd[,1] %in% ganet.ex.genes), 
                
                #which(hprd[,2]%in% ganet.ex.genes) ) )

#res <- ganet.FEtest(nOverlap=nrow(Validated), nPredicted=nrow(ganet.ex.net),
                     #nFocusedSet=nFocusedSet, nUniverse=nUniverse)

#res$stats$p.value
#res$precision
}

% R documentation directory.
\keyword{overlap}

