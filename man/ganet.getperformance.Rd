\name{ganet.getperformance}
\alias{ganet.getperformance}

\title{Performance results for Overlapping Significance}
\description{
  Determines a statistical p-value that shows the significance of the overlap between two networks. 
}
\usage{
  ganet.getperformance(PredictedNet, allgenes, literature)
}

\arguments{
  \item{PredictedNet}{
    The predicted network as two colums matrix.
  }
  \item{allgenes}{
    All the genes that are available in the begingin of the prediction analysis as unuique vector. 
  }
  \item{literature}{
    Optional. If not entered then all the literatyre combined automatically. Or you can input the database of interest as literature database as unique matrix of two columns. 
  }
}
\details{
  This may be used to practically get performance metrics, precision and p-value and others for the overlapping analysis.
}
\value{
  It results a vector that contains al the performance metrics. 
  \item{performances}{All the statistical results in a vector of the following outputs: "precision", "nOverlap (TP)", "nPredicted", "nFocusedSet", "nUniverse", "p.value" from hyper-geometric test.}

  
}
\references{

 1. G. Altay, J Zapardel, B Peters, "Determining preprocessing combinations and sample size for Gene Network Inference over ...", bioArxiv, 2017.

}
\author{
  Gokmen Altay
}

\seealso{
\code{\link{ganet.ComLinks}}, \code{\link{ganet.hyperg}},  \code{\link{ganet.combine}}, \code{\link{ganet.UniqNetSimp}}
}
\examples{
# data(ganet.ex.net)  # assume this is the predicted protein interactions

# Validated <- ganet.ComLinks(netlist=as.matrix(ganet.ex.net), netdata=as.matrix(hprd))
  
# data(ganet.ex.genes)  # load the example unique genes of the example 

#  res <- ganet.getperformance(PredictedNet=ganet.ex.net, allgenes=ganet.ex.genes

#performs overlap analysis over all the literature interactions  
# res
}

% R documentation directory.
\keyword{performance}

