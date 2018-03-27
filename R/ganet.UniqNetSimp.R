ganet.UniqNetSimp <- function(netlist, directed = FALSE)
{#1 and 2. columns are gene names and 3 is MI value, the rest is not important
  library(data.table)
  colnamesofnet <- colnames(netlist)
  netlist <- data.table(netlist)
  netlist[[1]] <- as.character(netlist[[1]])
  netlist[[2]] <- as.character(netlist[[2]])
  #
  netlist <- netlist[!( netlist[[1]]==netlist[[2]])] #remove dual links if any
  #
  if(directed == FALSE) netlist <- netlist[!duplicated(data.table(pmin(netlist[[1]],netlist[[2]]),
                                                                 pmax(netlist[[1]],netlist[[2]])))]

  if(directed != FALSE) netlist <-   netlist[!duplicated(data.table( netlist[[1]],netlist[[2]] ) )]
  
  
  colnames(netlist) <- colnamesofnet

  return(netlist)
}
