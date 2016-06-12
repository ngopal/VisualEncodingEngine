library(Rsymphony)
library(reshape)
library(plyr)

# Code to generate a random matrix of ranks (by row)
generateRankingMatrix <- function(nr,nc, binary = FALSE) {
  if (binary) {
    im <- matrix( round(runif(nc*nr, min=0, max=1)), nrow = nr)
    colnames(im) <- sprintf('task%d', 1:nc)
    rownames(im) <- sprintf('encoding%d', 1:nr)
    return( im )
  }
  else {
    im <- matrix( round(runif(nc*nr, min=1, max=nc)), nrow = nr)
    colnames(im) <- sprintf('task%d', 1:nc)
    rownames(im) <- sprintf('encoding%d', 1:nr)
    return( im )
  }
}


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

# to have an idea about column and row numbers
indices <- matrix(1:(num.tasks*num.encodings), num.encodings, num.tasks)
llook <- melt(indices)[which(lpr$solution == 1),]


# Objective 3 -- Given more tasks and less encodings, make the most efficient
# encoding assignments
objective3 <- generateRankingMatrix(3,5)
objective3 <- t(objective3)
linprog(objective3)


# Objective 2 -- Given less tasks and more encodings, make the optimal
# encoding assignments
objective2 <- generateRankingMatrix(5,3)
linprog(objective2)
which(linprog(objective2)$solution == 1)

# Even number assignments work as well
objective4 <- generateRankingMatrix(10,10)
linprog(objective4)
which(linprog(objective4)$solution == 1)

# Large matrix for objective 2
objective5 <- generateRankingMatrix(5,3)
linprog(objective5)
which(linprog(objective5)$solution == 1)

# Notes
# May want to column-normalize ranking tables prior to linear prog
# sweep(saveinput, 2, colSums(saveinput), FUN="/")
objective2Norm <- sweep(objective2, 2, colSums(objective2), FUN="/")
linprog(objective2Norm)
which(linprog(objective2Norm)$solution == 1)


# Benchmakring functions
library(rbenchmark)
benchmark(replications=rep(10, 3),
          linprog(generateRankingMatrix(5,3)),
          linprog(generateRankingMatrix(5,3)),
          columns=c('test', 'elapsed', 'replications'))
# 0.3 seconds

benchmark(replications=rep(10, 3),
          linprog(generateRankingMatrix(20,20)),
          linprog(generateRankingMatrix(20,20)),
          columns=c('test', 'elapsed', 'replications'))
# 3 seconds

benchmark(replications=rep(10, 3),
          linprog(generateRankingMatrix(25,25)),
          linprog(generateRankingMatrix(25,25)),
          columns=c('test', 'elapsed', 'replications'))
































# Bertin
# after adding parity constraints, could add
# 2 more columns: one for selective, one

# Task prioritization
# after adding parity constraints, could an additional priority columns,
# one for each task. Then, 
savemat <- mat
#c(rep(0.2,3),rep(0.1,3),rep(0.5,3),rep(0.05,3),rep(0.15,3))
mat <- rbind(mat, c(rep(0.2,3),rep(0,14)))
mat <- rbind(mat, c(rep(0,3),rep(0.1,3),rep(0,11)))
mat <- rbind(mat, c(rep(0,6),rep(0.5,3),rep(0,8)))
mat <- rbind(mat, c(rep(0,9),rep(0.05,3),rep(0,5)))
mat <- rbind(mat, c(rep(0,12),rep(0.15,3),rep(0,2)))

mat <- cbind(mat,0)
dir <- c(dir, rep(">=", 5))
rhs <- c(rhs, rep(0, 5))

types <- c(rep("B", 15), rep("C", 2), "C")
max <- TRUE # maximizing utility

saveinput = inputMatrix
inputMatrix <- c(as.numeric(inputMatrix), parity, 0)

Rsymphony_solve_LP(inputMatrix, mat, dir, rhs, types=types, max=max)
soln <- Rsymphony_solve_LP(inputMatrix, mat, dir, rhs, types=types, max=max)


# This right here is how I can optimize for task weight!
# I need to use the COST coefficitions in the objective function
# for example, if there are 5 tasks, then I should have 5 normalized coefficients that represent priority
types <- c(rep("B", 15), rep("C", 2))
max <- TRUE # maximizing utility
Rsymphony_solve_LP(c(c(rep(0.2,3),rep(0.1,3),rep(0.5,3),rep(0.05,3),rep(0.15,3)), -1, 1), mat, dir, rhs, types=types, max=max)





Rsymphony_solve_LP(inputMatrix, mat, dir, rhs, types=types, max=TRUE)






library(RJSONIO)

testtable <- fromJSON('~/Code//VisualEncodingEngine/testinput.json');

#Tasks
testtable[[1]][2:length(testtable[[1]])]
#Encodings
sapply(testtable, function(x) x[[1]])[2:length(testtable)]

ddata <- c()
for (i in 2:length(testtable)) {
  ddata <- rbind(ddata, sapply(testtable, function(x) x[i])[2:length(testtable)])
}

# Ensure the matrix is in the same order as the table on the webpage
ddata <- t(ddata)

ddata[,1] <- floor(runif(length(ddata[1,]), min=1, max=5))
ddata[,2] <- floor(runif(length(ddata[1,]), min=1, max=5))
ddata[,3] <- floor(runif(length(ddata[1,]), min=1, max=5))
ddata[,4] <- floor(runif(length(ddata[1,]), min=1, max=5))
ddata[,5] <- floor(runif(length(ddata[1,]), min=1, max=5))

tddata <- data.frame(ddata)
#ddata <- fromJSON('~/Code//VisualEncodingEngine/jsserver//savedinput/1465250417970.json');
tddata <- as.matrix(ddata)
colnames(tddata) <- testtable[[1]][2:length(testtable[[1]])]
rownames(tddata) <- sapply(testtable, function(x) x[[1]])[2:length(testtable)]

tddata <- matrix(as.numeric(tddata), dim(tddata)[1], dim(tddata)[2])
lpres <- linprog(tddata)
tmdata <- matrix(lpres$solution[1:as.numeric(dim(tddata)[1]*dim(tddata)[2])], 
                 dim(tddata)[1], dim(tddata)[2])

colnames(tmdata) <- testtable[[1]][2:length(testtable[[1]])]
rownames(tmdata) <- sapply(testtable, function(x) x[[1]])[2:length(testtable)]

toJSON(as.data.frame(tmdata))




#### Understand iGraph structure for my own format ####
library(igraph)
g <- make_graph(c(1, 2, 2, 3, 3, 4, 5, 6), directed = FALSE)

vcount
ecount
# these would be attributes
temp <- get.edgelist(g)
temp <- cbind(temp, floor(runif(dim(temp)[1], min=1, max=dim(temp)[1])))
colnames(temp) <- c("from", "to", "weight")


function toGAP(item) {
  # edge attributes
  temp <- get.edgelist(g)
  temp <- cbind(temp, floor(runif(dim(temp)[1], min=1, max=dim(temp)[1])))
  colnames(temp) <- c("from", "to", "weight")
}






