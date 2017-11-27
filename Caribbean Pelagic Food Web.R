library(pdftools)
# This example uses the two page Appendix 4 of "SCIENTIFIC BASIS FOR ECOSYSTEM-BASED
# MANAGEMENT IN THE LESSER ANTILLES INCLUDING INTERACTIONS WITH MARINE MAMMALS
# AND OTHER TOP PREDATORS; A TROPHIC MODEL OF THE LESSER ANTILLES PELAGIC
# ECOSYSTEM" by  Mohammed et al. 2008

txt <- pdf_text('Raw Data/Mohammed_et_al_2008_Table.pdf')
df1 <- read.fwf(textConnection(txt[1]), c(6, 32, rep(7, 15)), skip = 2)
df2 <- read.fwf(textConnection(txt[2]), c(8, 32, rep(7, 3), 8, rep(7, 11)), skip = 2)
df <- cbind(df1[1:31, ], df2[1:31, 3:17])
colnames(df) <- c('Group', 'Functional.Group', 1:30)

# Remove first 2 columns to have just the matrix
df <- dplyr::select(df, -Group, -Functional.Group)

# Add 1 column to make it a square matrix (Dietritus not accounted for in column)
df <- cbind(df, 0)
colnames(df) <- c(267:297)
rownames(df) <- c(267:297)

###Replace NAs w/ 0's
df[is.na(df)] <- 0

##Replace links w/ 1's
df[df > 0] <- 1

##Remove self predation
#for (i in 1:nrow(df)) {
#  df[i,i] <- 0
#}

#######################
##Need to transpose the dataframe so that the matrix goes from predator (rows) to prey (cols)
#######################
Pelagic_T <- as.data.frame(t(df))

#converts object type to a standard matrix
Pelagic_Matrix <- as.matrix(Pelagic_T)
#creates a graph object
Pelagic_Graph <- igraph::graph_from_adjacency_matrix(Pelagic_Matrix)
#creates an edgelist
Pelagic_EdgeList <- igraph::get.edgelist(Pelagic_Graph)
#turns the edgelist into a dataframe
Pelagic_EdgeList_DF_Num <- as.data.frame(Pelagic_EdgeList)

#rename col names Predator & Prey (w/ Functional Groups)
colnames(Pelagic_EdgeList_DF_Num) <- c("Predator", "Prey")


## READIN Mohammed et al 20008 species list (full table can be subset to create multiple 'look-up' tables )
txt <- pdf_text('Raw Data/Mohammed_et_al_2008_SpeciesList.pdf')
widths <- list(
  c(7, 31, 28, 30),
  c(7, 24, 45, 30),
  c(7, 23, 39, 30)
)
df <- data.frame()
for (i in 1:length(txt)) {
  con <- textConnection(txt[i])
  dfi <- read.fwf(con, widths[i], skip = c(4, 3, 3)[i])
  df <- rbind(df, dfi[1:c(52, 52, 51)[i], ])
}

Pelagic_Table <- df

##change col names 
colnames(Pelagic_Table) <- c("Trophospecies_Number", "Functional.Group", "Coded.As", "Scientific.Name")
Pelagic_Table$Functional.Group <- trimws(Pelagic_Table$Functional.Group)
Pelagic_Table$Coded.As <- trimws(Pelagic_Table$Coded.As)
Pelagic_Table[Pelagic_Table == ""] <- NA

Pelagic_Table <- fill(Pelagic_Table, Trophospecies_Number, Functional.Group)

Pelagic_Table[is.na(Pelagic_Table)] <- ""

##change functional group numbers to be 267 through 297
Pelagic_Table$Trophospecies_Number <- Pelagic_Table$Trophospecies_Number + 266

#########################
##changed tropho number for species that are also in the reef web
#########################
## mackerel species that are in the reef web
Pelagic_Table[Pelagic_Table$Coded.As == "King mackerel", "Trophospecies_Number"] <- Common_Tropho[Common_Tropho$Coded.As == "King Mackerel", "Trophospecies_Number"]
Pelagic_Table[Pelagic_Table$Coded.As == "Cero mackerel", "Trophospecies_Number"] <- Common_Tropho[Common_Tropho$Coded.As == "Cero Mackerel", "Trophospecies_Number"]

## Coastal predators #285 that are in the reef web 
Pelagic_Table[Pelagic_Table$Coded.As == "Bar jack", "Trophospecies_Number"] <- Common_Tropho[Common_Tropho$Coded.As == "Bar Jack", "Trophospecies_Number"]
Pelagic_Table[Pelagic_Table$Coded.As == "Yellowtail snapper", "Trophospecies_Number"] <- Common_Tropho[Common_Tropho$Coded.As == "Yellowtail Snapper", "Trophospecies_Number"]
Pelagic_Table[Pelagic_Table$Coded.As == "Great barracuda", "Trophospecies_Number"] <- Common_Tropho[Common_Tropho$Coded.As == "Great Barracuda", "Trophospecies_Number"]

##CREATE DATAFRAME - PELAGIC_COMM_SCI_IDNUM
Pelagic_Comm_Sci_IDnum <- dplyr::select(Pelagic_Table, Coded.As, Scientific.Name, Trophospecies_Number)

##CREATE DATEFRAME - Pelagic_FG-IDnum 
Pelagic_FG_IDnum <- dplyr::select(Pelagic_Table, Functional.Group, Trophospecies_Number)

##CREATE DATEFRAME - Pelagic_Comm-FG
Pelagic_Comm_FG <- dplyr::select(Pelagic_Table, Coded.As, Functional.Group)

##CREATE DATAFRAME - Pelagic_Comm-Sci
Pelagic_Comm_Sci <- dplyr::select(Pelagic_Table, Coded.As, Scientific.Name)

##CREATE DATAFRAME - Pelagic_Comm-IDnum
Pelagic_Comm_IDnum <- dplyr::select(Pelagic_Table, Coded.As, Trophospecies_Number)


###################### RECREATING PELAGIC FOOD-WEB################
##Original Edgelist = Pelagic_EdgeList_DF_Num

##Comparing pelagic and reef species lists
SharedSpp <- dplyr::inner_join(JA_Trophospecies, Pelagic_Table, by = c('Taxa' = 'Scientific.Name'))

source("Disaggregator_Function.R")
##Disaggregates node 285 (coastal pelagics)
Pelagic_EdgeList_DF_Num <- disaggregator(Pelagic_EdgeList_DF_Num, 285, c(232, 252, 255))
##Disaggregates node 280 (mackerels)
Pelagic_EdgeList_DF_Num <- disaggregator(Pelagic_EdgeList_DF_Num, 280, c(239, 248))

#Convert 'chr' to 'num' for all
Pelagic_EdgeList_DF_Num$Predator <- as.numeric(Pelagic_EdgeList_DF_Num$Predator)
Pelagic_EdgeList_DF_Num$Prey <- as.numeric(Pelagic_EdgeList_DF_Num$Prey)


