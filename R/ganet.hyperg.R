ganet.hyperg <- function(nOverlap=c(), nPredicted=c(), nReference=c(), nUniverse=c() )
{#Assumes all the matrices have unique links. It only considers the first and second columns.
if(is.null(nOverlap)) 	return(print("Enter the nOverlap argument for the number of overlap and try again."))
if(is.null(nPredicted)) 	return(print("Enter the nPredicted argument for the number of predictions and try again."))
if(is.null(nReference)) 	return(print("Enter the nReference argument for  the number of links in the database and try again."))
if(is.null(nUniverse)) 	return(print("Enter the nUniverse argument for the number of possible interactions of the raw dataset and try again."))
p.val.hyperg <- phyper( (nOverlap-1), nReference, (nUniverse-nPredicted), nPredicted , lower.tail = FALSE, log.p = FALSE) 
precision <- nOverlap/nPredicted

res <- new.env() 
assign("p.val.hyperg", p.val.hyperg, envir=res)
assign("precision", precision, envir=res)
return(res)
}

