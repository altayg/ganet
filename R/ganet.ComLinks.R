ganet.ComLinks <- function(netlist, netdata)
{#assumes the first and second columns have genes for interaction
  #e.g. assume netdata is a interaction database
  #netlist is the prediction set
  netlist <-as.matrix(netlist)
  netdata <- as.matrix(netdata)
  #
  a <- paste(netlist[,1],netlist[,2])
  b <- paste(netlist[,2],netlist[,1])
  
  x<- paste(netdata[,1], netdata[,2])
  
  i1<- which(a %in% x)
  i2<- which(b %in% x)
  ind <- union(i1,i2)
  res <- cbind(netlist[ind,],ind)
  return(res)
}
