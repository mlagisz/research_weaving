library(bibliometrix, dplyr)	
library(readxl)

# Read bib file
tmp <- readFiles("savedrecs.bib")
# Convert to a bibliometric data frame
bib <- convert2df(tmp, dbsource = "isi", format = "bibtex")
test <- bib$AU
test2 <- unlist(strsplit(test, ";"))
test2 <- test2[order(test2)]
#View(test2) #see which names need cleaning
write.csv(test2, "author_list_uncleaned.csv", row.names = FALSE)

# Do some cleaning (fix author initials)
test3 <- paste(test, ";", sep="")
corrected <- gsub("ALLISON D;", "ALLISON DB;", test3, ignore.case = TRUE)
corrected <- gsub("ANDERSON R;", "ANDERSON RM;", corrected, ignore.case = TRUE)
corrected <- gsub("AUSTAD S;", "AUSTAD SN;", corrected, ignore.case = TRUE)
corrected <- gsub("BROOKS R;", "BROOKS RC;", corrected, ignore.case = TRUE)
corrected <- gsub("CAREY J;", "CAREY JR;", corrected, ignore.case = TRUE)
corrected <- gsub("CARVALHO G;", "CARVALHO GB;", corrected, ignore.case = TRUE)
corrected <- gsub("DEBETS A;", "DEBETS AJM;", corrected, ignore.case = TRUE)
corrected <- gsub("HARSHMAN L;", "HARSHMAN LG;", corrected, ignore.case = TRUE)
corrected <- gsub("HOEKSTRA R;", "HOEKSTRA RF;", corrected, ignore.case = TRUE)
corrected <- gsub("INGRAM D;", "INGRAM DK;", corrected, ignore.case = TRUE)
corrected <- gsub("JENNIONS M;", "JENNIONS MD;", corrected, ignore.case = TRUE)
corrected <- gsub("KENNEDY B;", "KENNEDY BK;", corrected, ignore.case = TRUE)
corrected <- gsub("LAMMING D;", "LAMMING DW;", corrected, ignore.case = TRUE)
corrected <- gsub("LEROI A;", "LEROI AM;", corrected, ignore.case = TRUE)
corrected <- gsub("MAAS M;", "MAAS MFPM;", corrected, ignore.case = TRUE)
corrected <- gsub("MCMAHAN C;", "MCMAHAN CA;", corrected, ignore.case = TRUE)
corrected <- gsub("MUELLER H;", "MULLER HG;", corrected, ignore.case = TRUE)
corrected <- gsub("MULLER H;", "MULLER HG;", corrected, ignore.case = TRUE)
corrected <- gsub("PENDERGRASS W;", "PENDERGRASS WR;", corrected, ignore.case = TRUE)
corrected <- gsub("PIPER MDW;", "PIPER M;", corrected, ignore.case = TRUE)
corrected <- gsub("PLETCHER S;", "PLETCHER SD;", corrected, ignore.case = TRUE)
corrected <- gsub("SINCLAIR D;", "SINCLAIR DA;", corrected, ignore.case = TRUE)
corrected <- gsub("SMITH G;", "SMITH GK;", corrected, ignore.case = TRUE)
corrected <- gsub("SMITH M;", "SMITH MJ;", corrected, ignore.case = TRUE)
bib$AU <- corrected #add corrections

#save as a data frame
write.csv(bib, "bib_as_df.csv", row.names = FALSE)

#look whether some of these names are cited
grep("SINCLAIR", bib$CR)
grep("KAEBERLEIN", bib$CR)

##########################################################################################
# Preliminary descriptive analysis 
results <- biblioAnalysis(bib, sep = ";")
summary(object = results, k = 20, pause = FALSE)

#####################  Figure - SKIP  ####################################################
pdf(file = "bib_4descriptive.pdf", height = 8, width = 8, pointsize=10)
par(mfrow=c(2,2), mar = c(4,4,1,1))
plot(results, k = 20, pause=FALSE)
dev.off()

# What are the most frequently cited manuscripts?
CR <- citations(bib, field = "article", sep = ".  ")
str(CR)

#####################  Figure - CO-AUTHORS NETWORK  ####################################################
pdf(file="Figure_collaboration_network.pdf", width=8, height=8, pointsize=10)
par(mfrow=c(1,1), mar=c(0,0,0,0))
M <- metaTagExtraction(bib, Field = "CR_AU", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")
net <- networkPlot(NetMatrix, n = 100, cluster="walktrap", Title = "", labelsize = 1, type = "fruchterman", size=TRUE, remove.multiple=TRUE)	
dev.off()
###################################################################################

#Note: countries cannot be extracted from bib


#####################  Figure - HISTORICAL CO-CITATION NETWORK  ####################################################
pdf(file="Figure_hstorical_network.pdf", width=8, height=8, pointsize=10)
par(mfrow=c(1,1), mar=c(0,0,0,0))
M <- metaTagExtraction(bib, Field = "CR_AU", sep = ";")
histResults <- histNetwork(M, n=143, sep = ";") # "n" (number of most cited references to select) picked manually, larger numbers give error
histPlot(histResults, size=TRUE, arrowsize = 0.2) 
dev.off()
###################################################################################



### CITATIONS

#co-ciation network
M <- metaTagExtraction(bib, Field = "CR_AU", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "authors", sep = ";")
#NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
net <- networkPlot(NetMatrix, n = 100, type = "kamada", Title = "Co-Citation", labelsize=0.5) #odd?

## Bipartite network of the existing networkds
M <- metaTagExtraction(bib, Field = "CR_AU", sep = ";")
# Co-Citation network of the existing studies
CRnet <- cocMatrix(M, Field = "CR", sep = ".  ")
net <- networkPlot(CRnet, n = 50, labelsize = 0.3, size=TRUE, weighted = TRUE)	
##### Error in graph.adjacency.sparse(adjmatrix, mode = mode, weighted = weighted, : not a square matrix

### BIBLIOGRAPHIC COUPLING
NetMatrix <- biblioNetwork (M, analysis = "coupling", network = "references", sep = ";")
net <- networkPlot(NetMatrix, n = 1000, cluster="walktrap", Title = "Bibliographic Coupling Network", label = FALSE, labelsize = 1, type = "kamada", size=TRUE, remove.multiple=TRUE)	
# odd - too few connections 


###################################################################################

## THEMATIC MAP
M <- metaTagExtraction(bib, Field = "CR_AU", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
S <- normalizeSimilarity(NetMatrix, type = "association")
net <- networkPlot(S, n = 500, Title = "co-occurrence network", type="fruchterman", labelsize = 0.7, halo = FALSE, cluster = "walktrap",remove.isolates=FALSE, remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
#too dense dont use for plot (try letter with smaller n)

#####################  Figure - THEMATIC MAP  ####################################################
pdf(file="Figure_thematic_map.pdf",width=8,height=8,pointsize=10)
res <- thematicMap(net, NetMatrix, S)
plot(res$map)
dev.off()
#########################################################################

#####################  Figure - TOPIC NETWORK  ####################################################
pdf(file="Figure_topic_network.pdf",width=8,height=8,pointsize=10)
net <- networkPlot(S, n = 60, Title = "", type="fruchterman", labelsize = 0.7, halo = FALSE, cluster = "walktrap",remove.isolates=FALSE, remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
dev.off()
#########################################################################


#Conceptual structure
CS <- conceptualStructure(bib, field="AB", minDegree=4, k.max=10, stemming=FALSE, labelsize=10)	#too dense



###########################################################################
# Merge with bibliometric data with systematic map data
###########################################################################

#load processed bib data
bib <- read.csv("bib_as_df.csv")
str(bib)
unique(bib$SR) #143

#load systematic map data
dat <- read_excel("./ACEL_798_sm_DataS1_country.xls", sheet = 1)
names(dat)
dat$KEY <- paste(toupper(dat$FirstAuthor), dat$Year, dat$Journal, sep=" ") #create key
unique(dat$KEY)
dat$KEY[dat$Study_ID=="Paper111"] <- "MIN 2006 MECH AGEING DEV-a"

bib$SR #current key
bib$FA <- gsub("([A-Za-z]+).*", "\\1", bib$SR) #extract first author surename
bib$PY #publication year
bib$JI <- gsub("[.]", "", bib$JI) #(strip dots) #abbreviated journal name
#create new key
bib$KEY <- paste(toupper(bib$FA), bib$PY, bib$JI, sep=" ")
unique(bib$KEY) #142
bib$KEY[50] <- "MIN 2006 MECH AGEING DEV-a"
  
#check overlap
intersect(bib$KEY, dat$KEY) # 103 matching out of 143
setdiff(bib$KEY, dat$KEY) # 39 not matching in bib (1 missing!)
setdiff(dat$KEY, bib$KEY) # 40 not matching in dat

# Do some cleaning (fix journal names in the systematic map file)
corrected <- gsub("P NATL ACAD SCI USA", "PROC NATL ACAD SCI U S A", dat$KEY, ignore.case = TRUE)
corrected <- gsub("SAWADA 1987 J GERONTOL A-BIOL", "SAWADA 1987 J GERONTOL", corrected, ignore.case = TRUE)
corrected <- gsub("LE BOURG 1996 J GERONTOL A-BIOL", "LEBOURG 1996 J GERONTOL SER A-BIOL SCI MED SCI", corrected, ignore.case = TRUE)
corrected <- gsub("YU 1985 J GERONTOL A-BIOL", "YU 1985 J GERONTOL", corrected, ignore.case = TRUE)
corrected <- gsub("YU 1982 J GERONTOL A-BIOL", "YU 1982 J GERONTOL", corrected, ignore.case = TRUE)
corrected <- gsub("MURTAGH-MARK 1995 J GERONTOL A-BIOL", "MURTAGHMARK 1995 J GERONTOL SER A-BIOL SCI MED SCI", corrected, ignore.case = TRUE)
corrected <- gsub("YU 1982 J GERONTOL A-BIOL", "YU 1982 J GERONTOL", corrected, ignore.case = TRUE)
corrected <- gsub("J GERONTOL A-BIOL", "J GERONTOL SER A-BIOL SCI MED SCI" , corrected, ignore.case = TRUE)
corrected <- gsub("ARCH GERONTOL GERIAT", "ARCH GERONTOL GERIATR", corrected, ignore.case = TRUE)
corrected <- gsub("AM J PHYSIOL-REG I", "AM J PHYSIOL-REGUL INTEGR COMP PHYSIOL", corrected, ignore.case = TRUE)
corrected <- gsub("FREE RADICAL BIO MED", "FREE RADIC BIOL MED", corrected, ignore.case = TRUE)
corrected <- gsub("GENE DEV", "GENES DEV", corrected, ignore.case = TRUE)
corrected <- gsub("J EXP BIOL", "J EXP ZOOL", corrected, ignore.case = TRUE)
corrected <- gsub("FRESHWATER BIOL", "FRESHW BIOL", corrected, ignore.case = TRUE)
corrected <- gsub("VASSELLI 2005 OBESITY", "VASSELLI 2005 OBES RES", corrected, ignore.case = TRUE)
corrected <- gsub("YAMAKI 2005 BIOSCI BIOTECH BIOCH", "YAMAKI 2005 BIOSCI BIOTECHNOL BIOCHEM", corrected, ignore.case = TRUE)
corrected <- gsub("HONJOH 2008 NATURE", "HONJOH 2009 NATURE", corrected, ignore.case = TRUE)
corrected <- gsub("CHIPPINDALE 1993 J EVOLUTION BIOL", "CHIPPINDALE 1993 J EVOL BIOL", corrected, ignore.case = TRUE)
corrected <- gsub("INNESS 2008 P ROY SOC B-BIOL SCI", "INNESS 2008 PROC R SOC B-BIOL SCI", corrected, ignore.case = TRUE)
corrected <- gsub("SARMA 1991 INT REV HYDROBIOL", "SARMA 1991 INT REV GESAMTEN HYDROBIOL", corrected, ignore.case = TRUE)
corrected <- gsub("KASUMOVIC 2009 BIOL LETTERS", "KASUMOVIC 2009 BIOL LETT", corrected, ignore.case = TRUE)
corrected <- gsub("HOLLOSZY 1991 J APPL PHYSIOL" , "HOLLOSZY 1997 J APPL PHYSIOL" , corrected, ignore.case = TRUE)
corrected <- gsub("SHIMOKAWA 1993 J GERONTOL SER A-BIOL SCI MED SCI", "SHIMOKAWA 1993 J GERONTOL", corrected, ignore.case = TRUE)
corrected <- gsub("SHIMOKAWA 1991 J GERONTOL SER A-BIOL SCI MED SCI", "SHIMOKAWA 1991 J GERONTOL", corrected, ignore.case = TRUE)
corrected <- gsub("SNYDER 1990 J GERONTOL SER A-BIOL SCI MED SCI", "SNYDER 1990 J GERONTOL", corrected, ignore.case = TRUE)
corrected <- gsub("SONNTAG 1992 J GERONTOL SER A-BIOL SCI MED SCI", "SONNTAG 1992 J GERONTOL", corrected, ignore.case = TRUE)
corrected <- gsub("KUBO 1984 PROC NATL ACAD SCI U S A" , "KUBO 1984 NA" , corrected, ignore.case = TRUE)
corrected <- gsub("LENAERTS 2007 ANN NY ACAD SCI" , "LENAERTS 2007 ANNNY ACADSCI" , corrected, ignore.case = TRUE)
dat$KEY <- corrected #add corrections
#re-check overlap
intersect(bib$KEY, dat$KEY) # 143 matching out of 143
setdiff(bib$KEY, dat$KEY) # 0 not matching
setdiff(dat$KEY, bib$KEY) # 0 not matching

#save as a data frame
write.csv(bib, "bib_as_df_KEY.csv", row.names = FALSE)
write.csv(dat, "dat_as_df_KEY.csv", row.names = FALSE)

### MERGE (join)
dat_merged <- dplyr::full_join(dat, bib, by = "KEY")
str(dat_merged)
# full data + bib: may need to colapse to paper level for some future uses!
write.csv(dat_merged, "dat_merged.csv", row.names = FALSE)

#########################################

