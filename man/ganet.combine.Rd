\name{ganet.combine}
\alias{ganet.combine}

\title{Combines some or all of the availabe databases in it}
\description{
Conveiently combines some or all of the databases in the data dolder of the ganet package.
}
\usage{
ganet.combine(BIOGRID=1,CORUM=1,DIP=1,HPRD=1,
    INNATEDB=1,INTACT=1,MINT=1,MPI=1,NAT=0,REACT=0)
}

\arguments{
 
\item{BIOGRID}{BioGrid: Biological General Repository for Interaction Datasets. 515421 unique links.
  (Downloaded from http://thebiogrid.org/ by January 2015)
}

\item{CORUM}{CORUM: the Comprehensive Resource of Mammalian protein complexes. 44666 unique links.
(Downloaded from http://mips.helmholtz-muenchen.de/genre/proj/corum by January 2015)
}

\item{DIP}{DIP: Database of Interacting Proteins. 37945 unique links.
(Downloaded from http://dip.doe-mbi.ucla.edu/dip/Main.cgi by January 2015)
}

\item{HPRD}{Human Protein Reference Database (HPRD).
44376 unique links. Downloaded from http://www.hprd.org/download  by January 2015
}

\item{INNATEDB}{A knowledge resource for innate immunity interactions & pathways. 16665 unique links. 
Downloaded from http://www.innatedb.com/redirect.do?go=downloadImported  by January 2015
}

\item{INTACT}{IntAct provides protein interaction data. 136605 unique links. 
Downloaded from https://www.ebi.ac.uk/intact/ by January 2015
}

\item{MINT}{Molecular INTeraction database (MINT). 296806  links. 
Downloaded from http://www.pathwaycommons.org by Nov 2011
}

\item{MPI}{MIPS Mammalian Protein-Protein Interaction Database. 807 unique links.
Downloaded from http://mips.helmholtz-muenchen.de/proj/ppi/ by  January 2015
}

\item{NAT}{Nature Pathway Interaction Database. 12944 links. 
Downloaded from http://www.pathwaycommons.org by Nov 2011
}

\item{REACT}{Reactome: a knowledgebase of biological processes. 
642272 links. Downloaded from http://www.reactome.org/pages/download-data/ by January  2015
}


}
\details{
All the self interactions removed. Mutual interactions are unified. Please cite the reference of each database seperately for the ones you have used along with ganet references.

}
\value{
 \item{CombinedDatabases}{Combined and unified databases of the selected databases.}
 }
\references{
1. G. Altay, N. Altay, D.E. Neal, Global assessment of network inference algorithms based on available literature of gene/protein interactions, Turk J Biol, 37:547-555, 2013.
2. G. Altay, J Zapardel, B Peters, "Determining preprocessing combinations and sample size for Gene Network Inference over ...", bioArxiv, 2017.

3. Ruepp A, Waegele B, Lechner M, Brauner B, Dunger-Kaltenbach I, Fobo G, Frishman G, Montrone C, Mewes HW: CORUM: the comprehensive resource of mammalian protein complexes-2009. Nucleic Acids Research 2010, 38:D497-D501

4. Breuer K, Foroushani AK, Laird MR, Chen C, Sribnaia A, Lo R, Winsor GL, Hancock REW, Brinkman FSL, Lynn DJ: InnateDB: systems biology of innate immunity and beyond-recent updates and continuing curation. Nucleic Acids Research 2013, 41:D1228-D1233

5. Salwinski L, Miller CS, Smith AJ, Pettit FK, Bowie JU, Eisenberg D: The Database of Interacting Proteins: 2004 update. Nucleic Acids Research 2004, 32:D449-D451

6. Wang K, Nemenman I, Banerjee N, Margolin AA, Califano A: Genome-wide discovery of modulators of transcriptional interactions in human B lymphocytes. Research in Computational Molecular Biology, Proceedings 2006, 3909:348-362.

7. Kerrien S, Aranda B, Breuza L, Bridge A, Broackes-Carter F, Chen C, Duesbury M, Dumousseau M, Feuermann M, Hinz U, et al: The IntAct molecular interaction database in 2012. Nucleic Acids Research 2012, 40:D841-D846.

8. Licata L, Briganti L, Peluso D, Perfetto L, Iannuccelli M, Galeota E, Sacco F, Palma A, Nardozza AP, Santonico E, et al: MINT, the molecular interaction database: 2012 update. Nucleic Acids Research 2012, 40:D857-D861.

9. Prasad TSK, Goel R, Kandasamy K, Keerthikumar S, Kumar S, Mathivanan S, Telikicherla D, Raju R, Shafreen B, Venugopal A, et al: Human Protein Reference Database-2009 update. Nucleic Acids Research 2009, 37:D767-D772.

10. Stark C, Breitkreutz BJ, Reguly T, Boucher L, Breitkreutz A, Tyers M: BioGRID: a general repository for interaction datasets. Nucleic Acids Research 2006, 34:D535-D539.

}

\author{
Gokmen Altay
}
\note{
This package is intended to be updated with respect to feedbacks and also with updates of the current databases.
}

\seealso{
\code{\link{ganet.ComLinks}},   \code{\link{ganet.FEtest}}, \code{\link{ganet.UniqNetSimp}}
}
\examples{
#data(ganet.ex.net)

#CombinedDatabases <- ganet.combine(BIOGRID=1,CORUM=1,DIP=0,HPRD=0,
#INNATEDB=0, INTACT=0, MINT=0, MPI=0, NAT=0,REACT=0) 

## BIOGRID and CORUM databases are selected and  the other databases 
#are already zero by default.

## if you want to combine all the interactions then: alldatabases <- ganet.combine()

#see the example of ganet.FEtest for the use of this combined database
}

% R documentation directory.
\keyword{overlap}
