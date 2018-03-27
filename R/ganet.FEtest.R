ganet.FEtest <- function(nOverlap=c(), nPredicted=c(), nReference=c(), nUniverse=c() )
{#Assumes all the matrices have unique links. It only considers the first and second columns.
  if(is.null(nOverlap))   return(print("Enter the nOverlap argument for the number of overlap and try again."))
  if(is.null(nPredicted)) 	return(print("Enter the nPredicted argument for the number of predictions and try again."))
  if(is.null(nReference)) 	return(print("Enter the nReference argument for  the number of links in the database and try again."))
  if(is.null(nUniverse)) 	return(print("Enter the nUniverse argument for the number of possible interactions of the raw dataset and try again."))
  
  fe1 <- nOverlap
  fe2 <- nPredicted - nOverlap
  fe3 <- nReference - nOverlap
  fe4 <- nUniverse - nPredicted + nOverlap 
  
  FisherData <- matrix(c(fe1, fe3, fe2, fe4), nrow=2)
  stats <- fisher.test(FisherData, alternative = "greater")
  stats$p.value   # p-val
  
  precision <- nOverlap/nPredicted
  precision #accuracy rate 
  
  res <- new.env() 
  assign("stats", stats, envir=res)
  assign("precision", precision, envir=res)
  return(res)
}
