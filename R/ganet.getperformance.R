ganet.getperformance <- function(PredictedNet=c(), allgenes=c() , literature=c(), uppercase=FALSE)
{#gives overlap analysis results easily. Assumes nPredicted has already unique interactions
  if(is.null(PredictedNet)) 	return(print("Enter the PredictedNet  argument for the predicted interactions in two columns."))
  if(is.null(allgenes)) 	return(print("Enter allgenes argument that are the set of unique genes while starting analysis and try again."))
  if(is.null(literature)) CombinedDatabases <- ganet.combine() else CombinedDatabases <- literature
  
  nUniverse <- length(allgenes)*(length(allgenes) - 1)/2 
  
  ###narrow down the reference list regarding our data
  CombinedDatabases <- as.data.table(CombinedDatabases)
  if(uppercase){
    CombinedDatabases[[1]] <- toupper(as.character(CombinedDatabases[[1]]))
    CombinedDatabases[[2]] <- toupper(as.character(CombinedDatabases[[2]]))
    allgenes <- toupper(as.character(allgenes))
  }
  # 
  CombinedDatabases <- CombinedDatabases[(V1 %in% allgenes) | (V2 %in% allgenes)]
  #
  nFocusedSet <- nrow(CombinedDatabases)
  ###
  nPredicted <- nrow(PredictedNet)
 
  ValidatedNet <- ganet.ComLinks(PredictedNet,CombinedDatabases, uppercase=uppercase)
  nOverlap= nrow(ValidatedNet)

  
  #phyper(nOverlap-1, nReference, nAllPossible-nReference, nPredicted, lower.tail = FALSE)
  
  p.val.hyperg <- phyper( (nOverlap-1), nFocusedSet, (nUniverse-nPredicted), nPredicted , lower.tail = FALSE, log.p = FALSE) 
  precision <- nOverlap/nPredicted

  performances <- c(precision, nOverlap, nPredicted, nFocusedSet, nUniverse, p.val.hyperg)
  names(performances) <- c("precision", "nOverlap (TP)", "nPredicted", "nFocusedSet", "nUniverse", "p.value")
  
  rm(i,i2,j,nUniverse,nFocusedSet, nPredicted,nOverlap,CombinedDatabases)
  
  return(performances)
}