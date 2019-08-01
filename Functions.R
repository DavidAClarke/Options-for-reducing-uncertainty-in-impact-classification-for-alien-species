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


