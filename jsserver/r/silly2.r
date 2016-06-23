#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE);

library(RJSONIO)
library(Rsymphony)
library(plyr)


linprog <- function(inputMatrix) {

  num.tasks <- dim(inputMatrix)[2]
  num.encodings <- dim(inputMatrix)[1]

  mat.0 <- inputMatrix*0
  mat.utility.0 <- inputMatrix*0
  mat <- laply(1:num.encodings, function(ii) { x <- mat.0; x[ii, ] <- 1; as.double(x) })
  dir <- rep('<=', num.encodings)
  rhs <- rep(1, num.encodings)

  print(mat)
  cat(dir,'\n')
  cat(rhs,'\n')

  # Parity is still an issue, so that needs to be fixed next.
  lambda <- 1
  parity <- c(-lambda, -(-lambda))
  # later on, we'll turn obj.utility into a vector, and tag these on to it

  # add two columns of 0s to the existing constraints, for parity
  mat <- cbind(mat, 0, 0)

  # now for those d.upper and d.lower variables
  # \forall p, \sum_i u_i x_{i,p} - d.upper \le 0
  # \forall p, \sum_i u_i x_{i,p} - d.lower \ge 0
  # so, two more rows per person
  d.constraint <- function(task, ul) { # ul = 1 for upper, 0 for lower
    x<- mat.utility.0
    x[, task ] <- 1
    x <- x * inputMatrix
    c(as.double(x), (if (ul) c(-1,0) else c(0,-1)))
  }
  mat <- rbind(mat, maply(expand.grid(task=1:num.tasks, ul=c(1,0)), d.constraint, .expand=FALSE))
  dir <- c(dir, c(rep('<=', num.tasks), rep('>=', num.tasks)))
  rhs <- c(rhs, rep(0, num.tasks*2))

  # now this is a mixed-integer problem; some Boolean constraints, some continuous
  num.bool.consts <- num.encodings * num.tasks
  num.cont.consts <- 2

  types <- c(rep('B', num.bool.consts), rep('C', num.cont.consts))
  max <- TRUE # maximizing utility

  # finally, create the longer object function matrix
  inputMatrix <- c(as.numeric(inputMatrix), parity)

  soln <- Rsymphony_solve_LP(inputMatrix, mat, dir, rhs, types=types, max=max)
  return(soln)

}



testtable <- fromJSON(args[1]);
print(testtable)
numEnc <- length(testtable)
numTask <- length(testtable[[1]])
jsonOutfile <- args[2];

#Tasks
# testtable[[1]][2:length(testtable[[1]])]
#Encodings
# sapply(testtable, function(x) x[[1]])[2:length(testtable)]

# I want my matrix to have more rows than columns to properly enforce constraints.

if (numEnc <= numTask) {
    # transpose matrix
    rankedJsonMatrix = matrix(unlist(testtable), nrow=numTask, ncol=numEnc)
    print(rankedJsonMatrix)
    colNames <- rankedJsonMatrix[1,]
    colNames <- colNames[2:length(colNames)]
    print(colNames)
    rowNames <- rankedJsonMatrix[,1]
    rowNames <- rowNames[2:length(rowNames)]
    print(rowNames)
    rankedJsonMatrix = matrix(as.numeric(rankedJsonMatrix[2:numTask,2:numEnc]), nrow=numEnc-1, ncol=numTask-1)

    print(rankedJsonMatrix)

    rownames(rankedJsonMatrix) <- colNames
    colnames(rankedJsonMatrix) <- rowNames

    print(rankedJsonMatrix)

    lpres <- linprog(t(rankedJsonMatrix))
    tmdata <- matrix(lpres$solution[1:length(rankedJsonMatrix)], nrow=numTask-1, ncol=numEnc-1)

    print(tmdata)

    colnames(tmdata) <- rownames(rankedJsonMatrix)
    rownames(tmdata) <- colnames(rankedJsonMatrix)
    tmdata <- t(tmdata)

    print(tmdata)
    #write(toJSON(as.data.frame(tmdata)), jsonOutfile);

    print("ran transpose")

} else {
    # run as is
    rankedJsonMatrix = t(matrix(unlist(testtable), nrow=numTask, ncol=numEnc))
    print(rankedJsonMatrix)
    colNames <- rankedJsonMatrix[1,]
    colNames <- colNames[2:length(colNames)]
    print(colNames)
    rowNames <- rankedJsonMatrix[,1]
    rowNames <- rowNames[2:length(rowNames)]
    print(rowNames)
    rankedJsonMatrix = matrix(as.numeric(rankedJsonMatrix[2:numEnc,2:numTask]), nrow=numEnc-1, ncol=numTask-1)

    colnames(rankedJsonMatrix) <- colNames
    rownames(rankedJsonMatrix) <- rowNames

    length(rankedJsonMatrix)

    print(rankedJsonMatrix)

    lpres <- linprog(rankedJsonMatrix)
    tmdata <- matrix(lpres$solution[1:length(rankedJsonMatrix)], nrow=numEnc-1, ncol=numTask-1)

    colnames(tmdata) <- colnames(rankedJsonMatrix)
    rownames(tmdata) <- rownames(rankedJsonMatrix)

    print(as.data.frame(tmdata))

    #write(toJSON(as.data.frame(tmdata)), jsonOutfile);
    print("ran as is")
}
