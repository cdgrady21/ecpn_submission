# Index functions
rm(list=ls())
# ICW

## from cyrus samii IC weighting

# Function to standardize columns of a matrix
# where you designate a standardization group
# (e.g., the control group in an experiment)
# with "sgroup", a logical vector.

matStand <- function(x, sgroup = rep(TRUE, nrow(x))){
  for(j in 1:ncol(x)){
    x[,j] <- (x[,j] - mean(x[sgroup,j]))/sd(x[sgroup,j]) # chris: demeans outcome, but by control group mean, not x group mean?  Then divides by control group sd, which I get.  But why demean by control group mean even for tr group members?
  }
  return(x)
}

# Function that takes in data in matrix format and returns
# (i) IC weights and (ii) ICW index.
# Weights can be incorporated using the "wgts" argument.
# The "revcols" argument takes a vector indicating which columns,
# if any, should have values reversed (that is, standardized 
# values are multiplied by -1) prior to construction of the index. 

icwIndex <- function(	xmat,
                      wgts=rep(1, nrow(xmat)),
                      revcols = NULL,
                      sgroup = rep(TRUE, nrow(xmat))){
  X <- matStand(xmat, sgroup)
  if(length(revcols)>0){
    X[,revcols] <-  -1*X[,revcols]
  }
  i.vec <- as.matrix(rep(1,ncol(xmat)))
  Sx <- cov.wt(X, wt=wgts)[[1]]
  weights <- solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)
  index <- t(solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)%*%t(X))
  return(list(weights = weights, index = index))
}

# Factor analysis function (R base does not have one. factanal() is MLE, which we don't want.) 
#source("http://www.stat.sc.edu/~habing/courses/530/fact.txt")




#save
save.image(file="index_fun.rda")
