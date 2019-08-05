#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                                                                #
#              TITLE: Uncertainty in alien insect impact assessments                             #
#              PhD CHapter: 1                                                                    #
#              Date started: 31/01/2019                                                          #
#              Author: David A Clarke                                                            #
#                                                                                                #         
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Chapter/project description


# Section 1----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                                                                #
#              SECTION 1: Setup for analysis                                                     #
#                                                                                                #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~# Clears the environment
rm(list=ls())

#~# Create directories
dir.create("Data")
dir.create("Outcome")
dir.create("Documents")
dir.create("Scripts")

#~# Source data
Rnd1 <- read.csv("data/Round1.csv", header = T, sep = ",") 
Rnd2 <- read.csv("data/Round2.csv", header = T, sep = ",")
Final <- read.csv("data/Final_Results.csv", header = T, sep = ",")
Uncert <- read.csv("data/CombUncert.csv", header = T, sep = ",")


#~# Load functions
# This loads all the functions in the functions script into the global environment.
source("Scripts/Functions.R")

#~# Load required packages - !!will need to alter pacakges here for me!!
# This function is convenient for adding packages. If they aren't currently installed they will be prior to loading.
packageList <- c( # Writing the package list out like this enables me to keep a better track of what you are loading.
  # GENERAL
  "gtools",
  # DATA PROCESSING
  "plyr", "reshape2", "tidyr", "DataCombine",
  # SPATIAL DATA
  # PLOTTING,
  "ggplot2","RColorBrewer","circlize", "plotrix", "gridExtra"
  # STATISTICS
  )

#~# This uses the loadLibrary() function to install/load packages
loadLibrary(packageList)


                                  #~# Round 1 #~#

#~# Including NA as a level for all variables
Rnd1 <- NA2fctlvl(Rnd1)
Rnd1$Species <- droplevels(Rnd1$Species, exclude = if(anyNA(levels(Rnd1$Species))) NULL else NA)

#~# Round 1 Assessment severity agreement/disagreement frequency matrix
Rnd1.mat <- matrix(table(Rnd1[,8:9]), nrow = 7, ncol = 7)
rownames(Rnd1.mat) <- c("DD", "MC", "MN", "MO", "MR", "MV", "NA")
colnames(Rnd1.mat) <- c("DD", "MC", "MN", "MO", "MR", "MV", "NA")

#~# Variation in mechanism agreement        
InsectMechanism.1 <- Rnd1[,3:5]
Am <- c("Apis mellifera", "Competition", "Interaction with other alien species")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Am, RowNum = 11)
InsectMechanism.1[10,3] <- "Competition"
Lt <- c("Lysiphlebus testaceipes", "Competition", "Parasitism")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Lt, RowNum = 13)
InsectMechanism.1[12,3] <- "Competition"
Lh <- c("Linepithema humile", "Competition", "Predation")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Lh, RowNum = 15)
InsectMechanism.1[14,3] <- "Competition"
Pc <- c("Pachycondyla chinensis", "Competition", "Predation")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Pc, RowNum = 17)
InsectMechanism.1[16,3] <- "Competition"
Vv <- c("Vespula vulgaris", "Competition", "Chemical/Physical/Structural impact on ecosystem")
levels(InsectMechanism.1$Assessor.2.Mechanism) <- c(levels(InsectMechanism.1$Assessor.2.Mechanism), "Chemical/Physical/Structural impact on ecosystem")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Vv, RowNum = 26)
InsectMechanism.1[25,3] <- "Predation"
Ds <- c("Drosophila subobscura", "Facilitation of native species", "Competition")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Ds, RowNum = 28)
InsectMechanism.1[27,2] <- "Competition"
Ma <- c("Megachile apicalis", "Interaction with other alien species", "Interaction with other alien species")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Ma, RowNum = 30)
InsectMechanism.1[29,] <- c("Megachile apicalis", "Competition", "Competition")
Mr <- c("Myrmica rubra", "Interaction with other alien species", "Predation")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Mr, RowNum = 32)
InsectMechanism.1[31,] <- c("Myrmica rubra", "Competition", "Competition")
Ta <- c("Technomyrmex albipes", "Interaction with other alien species", "Other")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Ta, RowNum = 34)
InsectMechanism.1[33,2] <- "Competition"
Mro <- c("Megachile rotundata", "Interaction with other alien species", "Competition")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Mro, RowNum = 36)
InsectMechanism.1[35,2] <- "Competition"
Ha <- c("Harmonia axyridis", "Predation", "Competition")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Ha, RowNum = 38)
InsectMechanism.1[37,2] <- "Competition"
Vve <- c("Vespa velutina", "Predation", "Competition")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Vve, RowNum = 40)
InsectMechanism.1[39,2] <- "Competition"
Pf <- c("Paratrechina fulva", "Predation", "Predation")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Pf, RowNum = 42)
InsectMechanism.1[41,] <- c("Paratrechina fulva", "Competition", "Competition")
Wa <- c("Wasmannia auropunctata", "Predation", "Predation")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Wa, RowNum = 44)
InsectMechanism.1[43,] <- c("Wasmannia auropunctata", "Competition", "Competition")
Sr <- c("Solenopsis richteri", "Predation", "None")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Sr, RowNum = 46)
InsectMechanism.1[45,] <- c("Solenopsis richteri", "Competition", "None")
Sg <- c("Solenopsis geminata", "Predation", "Predation")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Sg, RowNum = 48)
Sge <- c("Solenopsis geminata", "Herbivory", "Predation")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Sge, RowNum = 49)
InsectMechanism.1[47,] <- c("Solenopsis geminata", "Competition", "Predation")
Vp <- c("Vespula pensylvanica", "Predation", "Predation")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Vp, RowNum = 51)
Vpe <- c("Vespula pensylvanica", "Other", "Other")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Vpe, RowNum = 52)
InsectMechanism.1[50,] <- c("Vespula pensylvanica", "Competition", "Competition")
Tf <- c("Tetropium fuscum", "Herbivory", "Predation")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Tf, RowNum = 57)
InsectMechanism.1[56,] <- c("Tetropium fuscum", "Herbivory", "Competition")
Ay <- c("Aulacaspis yasumatsui", "Herbivory", "Other")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Ay, RowNum = 71)
InsectMechanism.1[70,] <- c("Aulacaspis yasumatsui", "Herbivory", "Herbivory")
Ac <- c("Andricus corruptrix", "Facilitation of native species", "Other")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Ac, RowNum = 81)
InsectMechanism.1[80,] <- c("Andricus corruptrix", "Herbivory", "Other")
Ak <- c("Andricus kollari", "Facilitation of native species", "Other")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Ak, RowNum = 83)
InsectMechanism.1[82,] <- c("Andricus kollari", "Herbivory", "Other")
Al <- c("Andricus lignicola", "Facilitation of native species", "Other")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Al, RowNum = 85)
InsectMechanism.1[84,] <- c("Andricus lignicola", "Herbivory", "Other")
Ag <- c("Aphis gossypii", "Interaction with other alien species", "Interaction with other alien species")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Ag, RowNum = 90)
InsectMechanism.1[89,3] <- "Herbivory"
Ac <- c("Apis cerana", "Other", "Interaction with other alien species")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Ac, RowNum = 102)
InsectMechanism.1[101,3] <- "Competition"
Pm <- c("Pheidole megacephala", "Predation", "Competition")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Pm, RowNum = 113)
InsectMechanism.1[112,3] <- "Predation"
Si <- c("Solenopsis invicta", "Predation", "Competition")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Si, RowNum = 119)
InsectMechanism.1[118,3] <- "Predation"
Agr <- c("Anoplolepis gracilipes", "Interaction with other alien species", "Competition")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Agr, RowNum = 121)
InsectMechanism.1[120,] <- c("Anoplolepis gracilipes", "Predation", "Predation")
Dc <- c("Diaphorina citri", "Herbivory", "Transmission of disease")
InsectMechanism.1 <- InsertRow(InsectMechanism.1, NewRow = Dc, RowNum = 129)
InsectMechanism.1[130,2] <- "Transmission of disease"
InsectMechanism.1$Assessor.1.Mechanism <- factor(InsectMechanism.1$Assessor.1.Mechanism, levels = c("Herbivory","Competition","Predation","Other","Transmission of disease","Parasitism","Interaction with other alien species","Facilitation of native species","Hybridisation","Chemical/Physical/Structural impact on ecosystem","None"))
InsectMechanism.1$Assessor.2.Mechanism <- factor(InsectMechanism.1$Assessor.2.Mechanism, levels = c("Herbivory","Competition","Predation","Other","Transmission of disease","Parasitism","Interaction with other alien species","Facilitation of native species","Hybridisation","Chemical/Physical/Structural impact on ecosystem","None"))


                                  #~# Round 2 #~#

#~# Including NA as a level for all variables
Rnd2 <- NA2fctlvl(Rnd2)
Rnd2$Species <- droplevels(Rnd2$Species, exclude = if(anyNA(levels(Rnd2$Species))) NULL else NA)

#~# Round 2 Assessment severity agreement/disagreement frequency matrix
Rnd2.mat <- matrix(table(Rnd2[,6:7]), nrow = 7, ncol = 7)
rownames(Rnd2.mat) <- c("DD", "MC", "MN", "MO", "MR", "MV","NA")
colnames(Rnd2.mat) <- c("DD", "MC", "MN", "MO", "MR", "MV","NA")

#~# Variation in mechanism agreement
levels(Rnd2$Assessor.2.Mechanism)[1] <- "Chemical/Physical/Structural impact on ecosystem"
InsectMechanism.2 <- Rnd2[,3:5]
Vvu <- c("Vespula vulgaris", "Predation", "Chemical/Physical/Structural impact on ecosystem")
InsectMechanism.2 <- InsertRow(InsectMechanism.2, NewRow = Vvu, RowNum = 22)
InsectMechanism.2[21,2] <- "Competition"
Mru <- c("Myrmica rubra", "Interaction with other alien species", "Predation")
InsectMechanism.2 <- InsertRow(InsectMechanism.2, NewRow = Mru, RowNum = 26)
InsectMechanism.2[25,] <- c("Myrmica rubra","Competition", "Competition")
Sge <- c("Solenopsis geminata", "Competition", "Predation")
InsectMechanism.2 <- InsertRow(InsectMechanism.2, NewRow = Sge, RowNum = 35)
Sgem <- c("Solenopsis geminata", "Competition", "Herbivory")
InsectMechanism.2 <- InsertRow(InsectMechanism.2, NewRow = Sgem, RowNum = 36)
InsectMechanism.2[34,3] <- "Competition"
Vpen <- c("Vespula pensylvanica", "Predation", "Predation")
InsectMechanism.2 <- InsertRow(InsectMechanism.2, NewRow = Vpen, RowNum = 38)
Vpens <- c("Vespula pensylvanica", "Other", "Other")
InsectMechanism.2 <- InsertRow(InsectMechanism.2, NewRow = Vpens, RowNum = 39)
InsectMechanism.2[37,] <- c("Vespula pensylvanica", "Competition", "Competition")
Tfu <- c("Tetropium fuscum", "Predation", "Herbivory")
InsectMechanism.2 <- InsertRow(InsectMechanism.2, NewRow = Tfu, RowNum = 44)
InsectMechanism.2[43,2] <- "Competition"
Aco <- c("Andricus corruptrix", "Facilitation of native species", "Other")
InsectMechanism.2 <- InsertRow(InsectMechanism.2, NewRow = Aco, RowNum = 67)
InsectMechanism.2[66,2] <- "Herbivory"
Ako <- c("Andricus kollari", "Facilitation of native species", "Other")
InsectMechanism.2 <- InsertRow(InsectMechanism.2, NewRow = Ako, RowNum = 69)
InsectMechanism.2[68,2] <- "Herbivory"
Ali <- c("Andricus lignicola", "Facilitation of native species", "Other")
InsectMechanism.2 <- InsertRow(InsectMechanism.2, NewRow = Ali, RowNum = 71)
InsectMechanism.2[70,2] <- "Herbivory"
Ace <- c("Apis cerana", "Other", "Interaction with other alien species")
InsectMechanism.2 <- InsertRow(InsectMechanism.2, NewRow = Ace, RowNum = 87)
InsectMechanism.2[86,3] <- "Competition"
InsectMechanism.2$Assessor.1.Mechanism <- factor(InsectMechanism.2$Assessor.1.Mechanism, levels = c("Herbivory","Competition","Predation","Other","Transmission of disease","Parasitism","Interaction with other alien species","Facilitation of native species","Hybridisation","Chemical/Physical/Structural impact on ecosystem","None"))
InsectMechanism.2$Assessor.2.Mechanism <- factor(InsectMechanism.2$Assessor.2.Mechanism, levels = c("Herbivory","Competition","Predation","Other","Transmission of disease","Parasitism","Interaction with other alien species","Facilitation of native species","Hybridisation","Chemical/Physical/Structural impact on ecosystem","None"))


                                #~# Final Results #~#
#~# Including NA as a level for all variables
Final <- NA2fctlvl(Final)

Final$ConfVal <- as.factor(Final$ConfVal)
Final$Confidence <- factor(Final$Confidence, levels = c("Low", "Medium", "High"), ordered = T)
Final$Severity <- factor(Final$Severity, levels = c("MV","MR","MO","MN","MC","DD","NA"), ordered = T)
Final$scientificName <- factor(Final$scientificName, levels = unique(Final$scientificName))
Final$Mechanism <- factor(Final$Mechanism, levels = c("Herbivory","Competition","Predation",
                                                      "Other","Transmission of disease","Parasitism","Interaction with other alien species",
                                                      "Facilitation of native species","Hybridisation","Chemical/Physical/Structural impact on ecosystem","None"), ordered = T)


                        #~# Uncertainty classification #~#

Uncert <- Uncert[,c(1:4)]
Uncert <- NA2fctlvl(Uncert)
Uncert$ErrorSource <- factor(Uncert$ErrorSource, 
              levels =c("1 Human error", "2 Incomplete information searches",
                        "3 Taxonomic ambiguity","4 Resolution of data and scaling of alien range",
                        "5 Baseline information on indigenous range", "6a Limitations of assessment framework",
                        "6b Limitations of assessment framework","7 Species designation as invasive",
                        "8 Documented data and knowledge not readily or widely accessible",
                        "9 Unclear mechanism and/or extent of impact", "10 Extrapolation of evidence",
                        "11 Non-adherence to assessment protocol","12 No apparent cause"))
Uncert$Uncertainty <- factor(Uncert$Uncertainty, 
                             levels = c("Context dependence","Measurement error",
                                        "Model uncertainty","Natural variation",
                                        "Subjective judgement","Subjective judgement as a result of lack of knowledge",
                                        "Systematic error","Systematic error as a result of lack of knowledge",
                                        "Vagueness"))

# Section 2----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                                                                #
#              SECTION 2: Analysis                                                               #
#                                                                                                #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Frequency of 1st round mechanism agreement
Mech.agree.1 <- agree(Rnd1[,4:5])

# Frequency of 1st round severity agreement
Sev.agree.1 <- agree(Rnd1[,8:9])

# Frequency of 1st round confidence agreement
conf.agree.1 <- agree(Rnd1[,10:11])

# Frequency of 2nd round mechanism agreement
Mech.agree.2 <- agree(Rnd2[,4:5])

# Frequency of 2nd round severity agreement
Sev.agree.2 <- agree(Rnd2[,6:7])

# Frequency of 2nd round confidence agreement
conf.agree.2 <- agree(Rnd2[,8:9])

# Overall agreement
Fir.rnd <- data.frame(Mech.agree.1,Sev.agree.1,conf.agree.1)
Total.1 <- Fir.rnd[Fir.rnd$Mech.agree.1==1 & Fir.rnd$Sev.agree.1==1 & Fir.rnd$conf.agree.1==1, ]
length(Total.1$Mech.agree.1)

Sec.rnd <- data.frame(Mech.agree.2,Sev.agree.2,conf.agree.2)
Total.2 <- Sec.rnd[Sec.rnd$Mech.agree.2==1 & Sec.rnd$Sev.agree.2==1 & Sec.rnd$conf.agree.2==1,]
length(Total.2$Mech.agree.2)

# Impact severity agreement round 1
NumSev(Rnd1.mat)

# Impact severity agreement round 2
NumSev(Rnd2.mat)

#~# Variation in mechanism agreement round 1----
InsMechMat.1 <- matrix(nrow = 100, ncol = 11)
rownames(InsMechMat.1) <- levels(InsectMechanism.1$Species)
colnames(InsMechMat.1) <- levels(InsectMechanism.1$Assessor.1.Mechanism)
InsMechMat.1[1:nrow(InsMechMat.1),1:ncol(InsMechMat.1)] <- 0

# 1 = agreement, 2 = disagreement
InsMechMat.1[1,c(1,11)] <- 2
InsMechMat.1[2,1] <- 1
InsMechMat.1[3,2] <- 1
InsMechMat.1[4,c(1,4,8)] <- 2
InsMechMat.1[5,c(1,4,8)] <- 2
InsMechMat.1[6,c(1,4,8)] <- 2
InsMechMat.1[7,c(1,4)] <- 2
InsMechMat.1[8,5] <- 1
InsMechMat.1[9,3] <- 1
InsMechMat.1[9,c(2,7)] <- 2
InsMechMat.1[10,1] <- 1
InsMechMat.1[11,1] <- 1
InsMechMat.1[12,1] <- 1
InsMechMat.1[13,7] <- 1
InsMechMat.1[13,1] <- 2
InsMechMat.1[14,c(2,4,7)] <- 2
InsMechMat.1[15,7] <- 2
InsMechMat.1[15,2] <- 1
InsMechMat.1[16,2] <- 1
InsMechMat.1[17,c(1,11)] <- 2
InsMechMat.1[18,4] <- 2
InsMechMat.1[18,1] <- 1
InsMechMat.1[19,c(1,2)] <- 2
InsMechMat.1[20,6] <- 1
InsMechMat.1[21,2] <- 1
InsMechMat.1[22,11] <- 1
InsMechMat.1[23,c(1,11)] <- 2
InsMechMat.1[24,1] <- 1
InsMechMat.1[25,2] <- 1
InsMechMat.1[26,11] <- 1
InsMechMat.1[27,c(1,2)] <- 2
InsMechMat.1[28,c(5,11)] <- 2
InsMechMat.1[29,c(1,2)] <- 2
InsMechMat.1[30,11] <- 1
InsMechMat.1[31,c(2,11)] <- 2
InsMechMat.1[32,1] <- 1
InsMechMat.1[33,1] <- 2
InsMechMat.1[33,5] <- 1
InsMechMat.1[34,2] <- 1
InsMechMat.1[34,8] <- 2
InsMechMat.1[35,2] <- 1
InsMechMat.1[36,2] <- 1
InsMechMat.1[36,3] <- 2
InsMechMat.1[37,1] <- 1
InsMechMat.1[38,5] <- 1
InsMechMat.1[39,1] <- 1
InsMechMat.1[40,1] <- 1
InsMechMat.1[41,c(2,3)] <- 2
InsMechMat.1[42,c(1,11)] <- 2
InsMechMat.1[43,2] <- 1
InsMechMat.1[43,3] <- 2
InsMechMat.1[44,1] <- 1
InsMechMat.1[45,c(1,4)] <- 2
InsMechMat.1[46,2] <- 1
InsMechMat.1[46,6] <- 2
InsMechMat.1[47,c(2,7)] <- 1
InsMechMat.1[48,2] <- 1
InsMechMat.1[48,7] <- 2
InsMechMat.1[49,3] <- 1
InsMechMat.1[50,6] <- 1
InsMechMat.1[51,c(2,7)] <- 2
InsMechMat.1[52,c(3,4)] <- 2
InsMechMat.1[53,2] <- 1
InsMechMat.1[53,c(3,7)] <- 2
InsMechMat.1[54,1] <- 1
InsMechMat.1[55,2] <- 1
InsMechMat.1[56,c(5,11)] <- 2
InsMechMat.1[57,2] <- 1
InsMechMat.1[57,3] <- 2
InsMechMat.1[58,c(2,3)] <- 1
InsMechMat.1[59,2] <- 1
InsMechMat.1[60,c(1,11)] <- 2
InsMechMat.1[61,2] <- 2
InsMechMat.1[61,3] <- 1
InsMechMat.1[62,c(2,4)] <- 2
InsMechMat.1[63,3] <- 1
InsMechMat.1[64,2] <- 1
InsMechMat.1[65,11] <- 1
InsMechMat.1[66,6] <- 1
InsMechMat.1[67,c(2,3)] <- 2
InsMechMat.1[68,1] <- 1
InsMechMat.1[69,c(5,11)] <- 2
InsMechMat.1[70,c(1,11)] <- 2
InsMechMat.1[71,11] <- 1
InsMechMat.1[72,1] <- 1
InsMechMat.1[73,c(1,11)] <- 2
InsMechMat.1[74,c(1,5)] <- 2
InsMechMat.1[75,c(1,2)] <- 2
InsMechMat.1[75,3] <- 1
InsMechMat.1[76,3] <- 1
InsMechMat.1[76,2] <- 2
InsMechMat.1[77,3] <- 1
InsMechMat.1[78,c(2,3,11)] <- 2
InsMechMat.1[79,c(2,4)] <- 2
InsMechMat.1[80,c(2,3)] <- 2
InsMechMat.1[81,c(2,4,7)] <- 2
InsMechMat.1[82,c(1,2,3)] <- 2
InsMechMat.1[83,c(1,11)] <- 2
InsMechMat.1[84,9] <- 1
InsMechMat.1[85,3] <- 1
InsMechMat.1[86,2] <- 1
InsMechMat.1[87,6] <- 1
InsMechMat.1[88,6] <- 1
InsMechMat.1[89,c(2,11)] <- 2
InsMechMat.1[90,c(1,4)] <- 2
InsMechMat.1[91,c(4,8)] <- 2
InsMechMat.1[92,c(4,8)] <- 2
InsMechMat.1[93,3] <- 2
InsMechMat.1[93,2] <- 1
InsMechMat.1[94,c(2,3)] <- 2
InsMechMat.1[95,c(2,3,4)] <- 1
InsMechMat.1[96,c(2,3,10)] <- 2
InsMechMat.1[97,c(2,3)] <- 1
InsMechMat.1[98,c(5,7)] <- 2
InsMechMat.1[99,c(1,5)] <- 2
InsMechMat.1[100,9] <- 1

InsMechMat.1 <- InsMechMat.1[order(InsMechMat.1[,1],InsMechMat.1[,2],InsMechMat.1[,3],
                                   InsMechMat.1[,4],InsMechMat.1[,5],InsMechMat.1[,6],
                                   InsMechMat.1[,7],InsMechMat.1[,8],InsMechMat.1[,9],
                                   InsMechMat.1[,10],InsMechMat.1[,11], decreasing = T),]

apply(InsMechMat.1, 2, table)

#~# Variation in mechanism agreement round 2----
InsMechMat.2 <- matrix(nrow = 100, ncol = 11)
rownames(InsMechMat.2) <- levels(InsectMechanism.2$Species)
colnames(InsMechMat.2) <- levels(InsectMechanism.2$Assessor.1.Mechanism)
InsMechMat.2[1:nrow(InsMechMat.2),1:ncol(InsMechMat.2)] <- 0

# 1 = agreement, 2 = disagreement
InsMechMat.2[c(1:2),1] <- 1
InsMechMat.2[3,2] <- 1
InsMechMat.2[4,c(1,4,8)] <- 2
InsMechMat.2[5,c(1,4,8)] <- 2
InsMechMat.2[6,c(1,4,8)] <- 2
InsMechMat.2[7,4] <- 1
InsMechMat.2[8,5] <- 1
InsMechMat.2[9,c(2,3)] <- 2
InsMechMat.2[c(10:12),1] <- 1
InsMechMat.2[13,7] <- 1
InsMechMat.2[14,c(2,4,7)] <- 2
InsMechMat.2[15,c(2,7)] <- 2
InsMechMat.2[16,2] <- 1
InsMechMat.2[17,c(1,11)] <- 2
InsMechMat.2[18,c(1,4)] <- 2
InsMechMat.2[19,c(1,2)] <- 2
InsMechMat.2[20,6] <- 1
InsMechMat.2[21,2] <- 1
InsMechMat.2[22,11] <- 1
InsMechMat.2[c(23:24),1] <- 1
InsMechMat.2[25,2] <- 1
InsMechMat.2[26,11] <- 1
InsMechMat.2[27,c(2,11)] <- 2
InsMechMat.2[28,5] <- 1
InsMechMat.2[29,c(1,2)] <- 2
InsMechMat.2[30,11] <- 1
InsMechMat.2[31,2] <- 1
InsMechMat.2[32,1] <- 1
InsMechMat.2[33,5] <- 1
InsMechMat.2[34,c(2,8)] <- 2
InsMechMat.2[35,2] <- 1
InsMechMat.2[36,c(2,3)] <- 2
InsMechMat.2[37,1] <- 1
InsMechMat.2[38,5] <- 1
InsMechMat.2[c(39:40),1] <- 1
InsMechMat.2[41,3] <- 1
InsMechMat.2[42,c(1,11)] <- 2
InsMechMat.2[43,c(2,3)] <- 2
InsMechMat.2[44,1] <- 1
InsMechMat.2[45,c(1,4)] <- 2
InsMechMat.2[46,2] <- 1
InsMechMat.2[c(47:48),c(2,7)] <- 2
InsMechMat.2[49,3] <- 1
InsMechMat.2[50,6] <- 1
InsMechMat.2[51,2] <- 1
InsMechMat.2[52,4] <- 1
InsMechMat.2[53,2] <- 1
InsMechMat.2[53,c(3,7)] <- 2
InsMechMat.2[54,1] <- 1
InsMechMat.2[55,2] <- 1
InsMechMat.2[56,5] <- 1
InsMechMat.2[57,2] <- 1
InsMechMat.2[58,c(2,3)] <- 2
InsMechMat.2[59,2] <- 1
InsMechMat.2[60,c(1,11)] <- 2
InsMechMat.2[61,c(2,3)] <- 2
InsMechMat.2[62,4] <- 1
InsMechMat.2[63,3] <- 1
InsMechMat.2[64,2] <- 1
InsMechMat.2[65,11] <- 1
InsMechMat.2[66,6] <- 1
InsMechMat.2[67,3] <- 1
InsMechMat.2[68,1] <- 1
InsMechMat.2[69,5] <- 1
InsMechMat.2[70,c(1,11)] <- 2
InsMechMat.2[71,11] <- 1
InsMechMat.2[72,1] <- 1
InsMechMat.2[c(73:74),c(1,11)] <- 2
InsMechMat.2[75,2] <- 1
InsMechMat.2[75,c(1,3)] <- 2
InsMechMat.2[c(76:77),3] <- 1
InsMechMat.2[78,c(2,3)] <- 2
InsMechMat.2[79,4] <- 1
InsMechMat.2[80,3] <- 1
InsMechMat.2[81,c(4,7)] <- 2
InsMechMat.2[82,c(1,2)] <- 2
InsMechMat.2[83,1] <- 1
InsMechMat.2[84,9] <- 1
InsMechMat.2[85,3] <- 1
InsMechMat.2[86,2] <- 1
InsMechMat.2[c(87:88),6] <- 1
InsMechMat.2[89,2] <- 1
InsMechMat.2[90,4] <- 1
InsMechMat.2[c(91:92),c(4,8)] <- 2
InsMechMat.2[c(93:94),c(2,3)] <- 2
InsMechMat.2[95,c(2,3,4)] <- 1
InsMechMat.2[96,c(2,3,10)] <- 2
InsMechMat.2[97,c(2,3)] <- 2
InsMechMat.2[98,5] <- 1
InsMechMat.2[99,1] <- 1
InsMechMat.2[100,9] <- 1
InsMechMat.2 <- InsMechMat.2[order(InsMechMat.2[,1],InsMechMat.2[,2],InsMechMat.2[,3],
                                   InsMechMat.2[,4],InsMechMat.2[,5],InsMechMat.2[,6],
                                   InsMechMat.2[,7],InsMechMat.2[,8],InsMechMat.2[,9],
                                   InsMechMat.2[,10],InsMechMat.2[,11], decreasing = T),]

apply(InsMechMat.2, 2, table)

# Frequencies of the error types by mechanism and severity
table(Uncert[,c(2,4)])

# Frequencies of the uncertainty types by mechanism and severity
table(Uncert[,c(3,4)])

# Section 3----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                                                                #
#              SECTION 3: Plotting                                                               #
#                                                                                                #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Mechanism variation in round 1
LongData <- melt(InsMechMat.1)
ggplot(LongData, aes(x = Var2, y = Var1)) + 
      geom_tile(aes(fill=value), colour = "black") +
  scale_fill_gradientn(colours = c("white", "darkgreen", "orangered")) +
  theme_bw() +
  theme(axis.line = element_line(colour = "grey90"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1),
        legend.position = "none") +
  scale_y_discrete(limits = rev(levels(LongData$Var1))) +
  xlab("Mechanism of impact") +
  ylab("Species")

# Mechanism variation in round 2
LongData.2 <- melt(InsMechMat.2)
ggplot(LongData.2, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value), colour = "black") +
  scale_fill_gradientn(colours = c("white", "darkgreen", "orangered")) +
  theme_bw() +
  theme(axis.line = element_line(colour = "grey90"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1),
        legend.position = "none") +
  scale_y_discrete(limits = rev(levels(LongData.2$Var1))) +
  xlab("Mechanism of impact") +
  ylab("Species")



#~# Chord diagrams for Round 1 and 2 severity results
colours <- c("magenta4", "turquoise4", "black", "brown4", "deepskyblue3", "seagreen4", "gray45")
par(mfrow = c(1,2))
par(mar = c(3,1,3,1))    
chordDiagram(Rnd1.mat, grid.col = colours, transparency = 0.5, annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.03, 0.03))
text(1,1,labels = "A")
chordDiagram(Rnd2.mat, grid.col = colours, transparency = 0.5, annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.03, 0.03))
text(1,1,labels = "B")

#~# Plot of final results
#Matcol <- brewer.pal(6, "Spectral")
ggplot(Final, aes(Mechanism, scientificName)) +
  geom_tile(aes(fill = Severity)) +
  geom_point(aes(size = ConfVal), shape = 21, fill = "beige", colour = "black") +
  scale_y_discrete(limits = rev(levels(Final$scientificName))) +
  #scale_fill_discrete(guide = guide_legend(reverse = T)) +
  scale_fill_manual(values = rev(c("gray45", "magenta4", "turquoise4", "black", "brown4", "deepskyblue3", "seagreen4"))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#~# Same plot without mechanisms
ggplot(Final, aes(Severity, scientificName)) +
  geom_tile(aes(fill = Severity)) +
  geom_point(aes(size = ConfVal), colour = "grey28") +
  scale_y_discrete(limits = rev(levels(Final$scientificName))) +
  scale_x_discrete(limits = rev(levels(Final$Severity))) +
  #scale_fill_discrete(guide = guide_legend(reverse = T)) +
  scale_fill_manual(values = c("gray45", "magenta4", "turquoise4", "black", "brown4", "deepskyblue3", "seagreen4")) +
  theme_bw()

                                      ## Uncertainty ##

# Removing NA's for graphical purposes
UncertErr.noNA <- Uncert[!is.na(Uncert$ErrorSource),]
Uncert.noNA <- Uncert[!is.na(Uncert$Uncertainty),]

# Horizontal bar chart of causes of uncertainty
ggplot(UncertErr.noNA, aes(x = ErrorSource, fill = AssessmentComponent)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  facet_wrap(~AssessmentComponent) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "none") +
  xlab("Cause of uncertainty") +
  ylab("Frequency") +
  scale_fill_manual(values = c("black","gray")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(limits = rev(levels(UncertErr.noNA$ErrorSource))) 


# Horizontal bar chart of types of uncertainty
ggplot(Uncert.noNA, aes(x = Uncertainty, fill = AssessmentComponent)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  facet_wrap(~AssessmentComponent) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "none") +
  xlab("Type of uncertainty") +
  ylab("Frequency") +
  scale_fill_manual(values = c("black","gray")) +
  scale_y_continuous(expand = c(0,0))


                                     ## Overall results ##

# Summary of 1st and 2nd round results
Agreement <- c(44, 34, 32, 10, 65, 70, 63, 36)
Component <- c("Mechanism","Severity","Confidence","Overall","Mechanism","Severity","Confidence","Overall")
Component <- factor(Component, levels = c("Mechanism", "Severity", "Confidence", "Overall"))
Rnd <- c(rep("One",4),rep("Two",4))
SumRes <- data.frame(Agreement, Component, Rnd)
ggplot(SumRes, aes(Component, Agreement, fill = Rnd)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.90, 0.85),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_y_continuous(breaks = c(seq(0,100,25)), 
                     labels = c(0,25,50,75,100), 
                     expand = c(0,0), 
                     limits = c(0,100)) +
  scale_fill_manual("Assessment\nround",values = c("One" = "black", "Two" = "grey")) +
  ylab("Assessor agreement (%)")

  
  

