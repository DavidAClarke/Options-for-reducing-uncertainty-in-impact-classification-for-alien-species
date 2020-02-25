#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                                                                #
#              FUNCTIONS: PhD Chapter 1, David A Clarke, 31/01/2019                              #
#                                                                                                #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#                      

#~#~# Function loads / installing packages required libraries----
loadLibrary <- function(packageList){
  
  packLoaded <- list.files(.libPaths())
  
  lapply(packageList,function(x){
    
    if(!(x %in% packLoaded)){install.packages(x)}
    
    require(x,character.only = TRUE)
    
  })
  
}
#~#~# Function converting NA to factor level----
NA2fctlvl <- function(df) {
  for(j in 1:ncol(df)) {
    levels(df[[j]]) <- c(levels(df[[j]]),"NA")
    df[[j]][is.na(df[[j]])] <- "NA"
  }
  return(df)
}

#~#~# Function for assessing agreement in 2 df columns----
agree <- function(df) {
  ag <- vector(length = nrow(df))
  for(i in 1:nrow(df)) {
    ag[i] <- ifelse(df[i,1] == df[i,2], 1, 0)
  }
  return(ag)
}

#~#~ Function for calculating total number of instances for an impact severity----
# Needs to be a square matrix
NumSev <- function(matrix) {
  vec <- vector(length = nrow(matrix))
  for(i in 1:nrow(matrix)) {
    vec[i] <- sum(matrix[i,]) + sum(matrix[,i]) - diag(matrix)[i]
  }
  return(vec)
}

#~#~ Function for creating a matrix from the values of a dataframe----

# I need something that takes in a df, where the levels of the columns (which are the same)
# become the columns in a new matrix. I need to create a new matrix where ncol is the
# length of the levels of the df column. Then, for a given row in the df, if the value is the same
# in both columns then the corresponding matrix under the relevant column (level of the value)
# will be 1. If the values in given row differ between columns, each corresponding column in the
# matrix will receive -1 in the given row (species). Every other value that doesn't have 1 or -1 is 0

# this isn't working
#df2mat <- function(df) {
#  m <- matrix(nrow = length(levels(df[,1])), ncol = length(levels(df[,2])))
#  rownames(m) <- levels(df[,1])
#  colnames(m) <- levels(df[,2])
#  for(i in 1:nrow(df)) {
#    for(j in 1:ncol(df)) {
#      for(a in 1:nrow(m)) {
#       for(b in 1:ncol(m)) {
#         ifelse(df[i,2] == df[i,3] & df[i,2] == colnames(m) &
#                   df[i,1] == rownames(m)[i], m[a,b] <- 1, m[a,b] <- 0)
#        }
#      }
#     } 
#    }
#  return(m)
#}




#~#~# Function converting impact categories to ranks----
Cat2Rnk <- function(Category) {
  if(Category == "DD") {1
  } else if(Category == "MC") {2
  } else if(Category == "MN") {3
  } else if(Category == "MO") {4
  } else if(Category == "MR") {5
  } else if(Category == "MV") {6
  }
}

#~#~# Function for converting impact ranks to normalised ranks----
Rnk2Norm <- function(df) {
  (df-1)/(max(df-1))
}



#~#~# Function for generating random combinations of agreement/disagreement (WORK IN PROGRESS)----

# Items <- c("DD", "MC", "MN", "MO", "MR", "MV") #this will be the 'n' in the combinations
# 
# df <- data.frame(DD.DD = integer(length = 0),
#                 DD.MC = integer(length = 0),
#                 MC.MC = integer(length = 0),
#                 DD.MN = integer(length = 0),
#                 MC.MN = integer(length = 0),
#                 MN.MN = integer(length = 0),
#                 DD.MO = integer(length = 0),
#                 MC.MO = integer(length = 0),
#                 MN.MO = integer(length = 0),
#                 MO.MO = integer(length = 0),
#                 DD.MR = integer(length = 0),
#                 MC.MR = integer(length = 0),
#                 MN.MR = integer(length = 0),
#                 MO.MR = integer(length = 0),
#                 MR.MR = integer(length = 0),
#                 DD.MV = integer(length = 0),
#                 MC.MV = integer(length = 0),
#                 MN.MV = integer(length = 0),
#                 MO.MV = integer(length = 0),
#                 MR.MV = integer(length = 0),
#                 MV.MV = integer(length = 0))
# 
# 
# for(i in 1:100)
# {
#   combs <- combinations(Items, 2, replace = TRUE, nsample = 100)
#   combs.df <- as.data.frame(combs)
#   combs.df <- combs.df %>% unite("It.", c(V1, V2))
#   combs.df <- as.data.frame(table(combs.df))
#   if(length(combs.df$Freq) != 21) {
#     combs <- combinations(Items, 2, replace = TRUE, nsample = 100)
#     combs.df <- as.data.frame(combs)
#     combs.df <- combs.df %>% unite("It.", c(V1, V2))
#     combs.df <- as.data.frame(table(combs.df))
#   } else df[i,] <- combs.df$Freq }
# 
# for(i in 1:100) 
#   {
#   if(is.na(df[i,]) == TRUE) {
#     combs <- combinations(Items, 2, replace = TRUE, nsample = 100)
#     combs.df <- as.data.frame(combs)
#     combs.df <- combs.df %>% unite("It.", c(V1, V2))
#     combs.df <- as.data.frame(table(combs.df))
#     if(length(combs.df$Freq) == 21){
#     df[i,] <- combs.df$Freq
#     } else repeat {
#       combs <- combinations(Items, 2, replace = TRUE, nsample = 100)
#       combs.df <- as.data.frame(combs)
#       combs.df <- combs.df %>% unite("It.", c(V1, V2))
#       combs.df <- as.data.frame(table(combs.df))
#       if(is.na(df[i,] == FALSE)){
#         break
#       }
#     }
#   }
# }




  

#Combs <- combinations(Items, 2, replace = TRUE, nsample = 100)
#combs.df <- as.data.frame(Combs)
#combs.df %>% unite("It.", c(V1, V2))
#Then either
#table(combs.df)
#or
#combs.df <- as.data.frame(table(combs.df))
#combs.df <- combs.df[combs.df$Freq > 0,]



#df[2,] <- Combs.df$Freq

#new.df <- function(df){
#  repeat{
#    Combs <- combinations(Items, 2, replace = TRUE, nsample = 100)
#    combs.df <- as.data.frame(Combs) #as_tibble
#    combs.df %>% unite("It.", c(V1, V2))
#    combs.df <- as.data.frame(table(combs.df)) #as_tibble
#    combs.df <- combs.df[combs.df$Freq > 0,] #filter(n > 0)
#    combs.df <- t(combs.df$Freq) #mutate?
#if(nrow(df) == 0) {
#  df <- DataCombine::InsertRow(df, combs.df, RowNum = (nrow(df)+1))
#} else if(nrow(df)!=0){
#  df <- DataCombine::InsertRow(df, combs.df, RowNum = (nrow(df)))
#} else if(nrow(df) == 101) {
#  print("The end");
#  break
#}
#  return(df)
#  }
#}



#Need to repeat many times i.e. many iterations and store in a neat way and then find means etc.