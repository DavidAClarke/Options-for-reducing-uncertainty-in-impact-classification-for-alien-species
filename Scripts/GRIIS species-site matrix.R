library(readxl)
GRIIS <- read_xlsx("C:/Users/dcla0008/Dropbox/PhD/Data/Chapter 1/Data/GRIIS_sTWIST_March_Final+Spain_DC.xlsx", col_names = T, trim_ws = T, n_max = 108958)

# Creating a species by site (countryCode)
GRIIS.min <- data.frame(GRIIS[,c(4,5)])
GRIIS.mat <- xtabs(~ scientificName + countryCode, GRIIS.min)
Site.Total <- colSums(GRIIS.mat)
Sp.Total <- rowSums(GRIIS.mat)
GRIIS.mat <- cbind(GRIIS.mat, Sp.Total)
GRIIS.mat <- rbind(GRIIS.mat, Site.Total)
# need to make sure the new taxon table lines up with species order in matrix
# What I have done here doesn't quite work properly
taxon <- GRIIS[,5:10]
taxon.min <- taxon[!duplicated(taxon$scientificName), ]
Total <- c(rep("Total", 6))
taxon.min <- rbind(taxon.min, Total)
GRIIS.mat <- cbind(GRIIS.mat, taxon.min)
write.csv(GRIIS.mat, file = "C:/Users/dcla0008/Dropbox/PhD/Data/Chapter 1/Data/GRIIS.mat.csv")

