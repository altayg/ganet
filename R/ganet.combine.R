ganet.combine <- function(BIOGRID=1, CORUM=1, DIP=1,HPRD=1, INNATEDB=1, INTACT=1,MINT=1,MPI=1, NAT=0,REACT=0)
{
  #databaseNames <- c("BCI","BioGrid","hprd","UniHI","intact","mint","nat","react")
  CombinedDatabases <- c()
  
  
  if(BIOGRID ==1) {data(biogrid); CombinedDatabases <- rbind(CombinedDatabases, biogrid[,1:2]);rm(biogrid)}
  if(CORUM ==1) {data(corum); CombinedDatabases <- rbind(CombinedDatabases, corum[,1:2]);rm(corum)}
  if(DIP ==1) {data(dip); CombinedDatabases <- rbind(CombinedDatabases, dip[,1:2]); rm(dip)}
  if(HPRD ==1)  {data(hprd); CombinedDatabases <- rbind(CombinedDatabases, hprd[,1:2]); rm(hprd)}
  if(INNATEDB ==1)  {data(innatedb); CombinedDatabases <- rbind(CombinedDatabases, innatedb[,1:2]); rm(innatedb)}
  if(INTACT ==1)  {data(intact);CombinedDatabases <- rbind(CombinedDatabases, intact[,1:2]); rm(intact)}
  if(MINT ==1)  {data(mint);CombinedDatabases <- rbind(CombinedDatabases, mint[,1:2]); rm(mint)}
  if(MPI ==1)  {data(mpi);CombinedDatabases <- rbind(CombinedDatabases, mpi[,1:2]); rm(mpi)}
  #pathways are the followings
  if(NAT ==1)  {data(nat);CombinedDatabases <- rbind(CombinedDatabases, nat[,1:2]); rm(nat) }
  if(REACT ==1) {data(reactome); CombinedDatabases <- rbind(CombinedDatabases, reactome[,1:2]); rm(reactome)}

  CombinedDatabases <- ganet.UniqNetSimp(CombinedDatabases)
  
  return(CombinedDatabases)
}
