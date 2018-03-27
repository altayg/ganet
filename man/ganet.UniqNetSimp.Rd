\name{ganet.UniqNetSimp}
\alias{ganet.UniqNetSimp}

\title{Unifying a net list}
\description{
Unifies the same links in the given net list.
}
\usage{
ganet.UniqNetSimp(netlist)
}

\arguments{
  \item{netlist}{Considers the first and second columns only.}
}

\details{
All the same interactions are unified.
 
}
\value{
 \item{netlist}{Unified netlist}
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
\code{\link{ganet.ComLinks}},   \code{\link{ganet.FEtest}}, \code{\link{ganet.combine}}
}
\examples{
#data(ganet.ex.net)  # assume this is the predicted protein interactions

#ganet.ex.net <- ganet.UniqNetSimp(ganet.ex.net)  #unify the same links

}

% R documentation directory.
\keyword{overlap}

