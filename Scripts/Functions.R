#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                                          #
# TITLE: An analysis of uncertainty and options for reducing it in impact  #
#        assessment for alien species                                      #
# PhD CHapter: 1                                                           #
# Script author: David A Clarke                                            #
#                                                                          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  

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



