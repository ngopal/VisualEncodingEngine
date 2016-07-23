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

library(rgexf)
igraph.to.gexf(g, position=NULL)

function toCytoJSON(g) {
  s = c()
  s = append(s,"[")
  for (i in 1:length(V(g))) {
    s = append(s, paste("{ data: { id: \'",i,"\' } },", sep=''))
  }
  temp <- get.edgelist(g)
  for (i in 1:length(E(g))) {
    if (i == length(E(g))) {
      s = append(s, paste("{ data: { id: \'",i+length(V(g)),"\', source: \'",temp[i,1],"\', target: \'",temp[i,2],"\' } }", sep=''))
    }
    else {
      s = append(s, paste("{ data: { id: \'",i+length(V(g)),"\', source: \'",temp[i,1],"\', target: \'",temp[i,2],"\' } },", sep=''))
    }  
  }
  s = append(s, "]")
  return( paste(s, collapse="") )
}












linprogNew <- function(inputMatrix) {
  
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
  
  # add custom num constraint
  mat <- rbind(mat, 1)
  mat[dim(mat)[1],dim(mat)[2]-1] <- 0
  mat[dim(mat)[1],dim(mat)[2]] <- 0
  
  dir <- c(dir, "<=")
  rhs <- c(rhs, 3)
  
  # now this is a mixed-integer problem; some Boolean constraints, some continuous
  num.bool.consts <- num.encodings * num.tasks
  num.cont.consts <- 2
  
  types <- c(rep('B', num.bool.consts), rep('C', num.cont.consts), 'C')
  max <- TRUE # maximizing utility
  
  # finally, create the longer object function matrix
  inputMatrix <- c(as.numeric(inputMatrix), parity)
  
  soln <- Rsymphony_solve_LP(inputMatrix, mat, dir, rhs, types=types, max=max)
  return(soln)
  
}


samp_mat <-c()
for (i in 1:100) {
  samp_mat <- rbind(samp_mat,sample(1:14))
}
samp_mat <- matrix(samp_mat, 100, 14)
samp_out <- sample(1:100)
samp_out <- rep(sample(0:1),50)
samp_lm <- glm(samp_out ~ ., data=data.frame(samp_mat), family = "gaussian")
samp_lm <- glm(samp_out ~ ., data=data.frame(samp_mat), family = "binomial")
plot(samp_lm)




## Load in GeneMANIA example into Cytoscape form
genemania.network <- read.delim("~/Code/VisualEncodingEngine/data/genemania-network-BRCA.txt", skip=6, header = TRUE)
genemania.network <- read.delim("~/Code/VisualEncodingEngine/data/genemania-network-APP.txt", skip=6, header = TRUE)
View(genemania.network)
colnames(genemania.network) <- c("Entity.1", "Entity.2", "weight", "group", "publication")
genemania.network.graph <- graph.data.frame(genemania.network, directed=FALSE)
V(genemania.network.graph)$area <- pubMedCounts(genemania.network.graph)

tpyFunc <- function(g) {
  s = c();
  s = append(s, '[')
  vert.attrs <- list.vertex.attributes(g);
  edge.attrs <- list.edge.attributes(g);
  for (i in 1:length(V(g))) {
#     cat('{ data: { id: \"',i,'\", ')
    s = append(s, paste('{ data: { id: "',i,'", ', sep=''))
    for (k in 1:length(vert.attrs)) {
      if (k==length(vert.attrs)) {
        s = append(s, paste(vert.attrs[k],' : "', eval(parse(text=paste("V(g)$",vert.attrs[k],'[',i,']',sep=''))),'" ', sep='') )
#         cat(vert.attrs[k],' : \"', eval(parse(text=paste("V(g)$",vert.attrs[k],'[',i,']',sep=''))),'\" ' )
      }
      else {
        s = append(s, paste(vert.attrs[k],' : "', eval(parse(text=paste("V(g)$",vert.attrs[k],'[',i,']',sep=''))),'", ', sep='') )
#         cat(vert.attrs[k],' : \"', eval(parse(text=paste("V(g)$",vert.attrs[k],'[',i,']',sep=''))),'\", ' )
      }
    }
    s = append(s, paste(' } },', sep=''))
#     cat(' } },', '\n')
  }
  for (e in 1:length(E(g))) {
    pairs = get.edgelist(g)
    for (k in 1:length(edge.attrs)) {
#       cat('{ data: { id: \"',i+e,'\", ')
#       cat('source: \"',  which(V(g)$name == get.edgelist(g)[e,1])  ,'\", target: \"',  which(V(g)$name == get.edgelist(g)[e,2])  ,'\", ')
#       cat('dimension: \"', edge.attrs[k],'\", value:\"',eval(parse(text=paste("E(g)$",edge.attrs[k],'[',e,']',sep=''))),'\"' )
#       cat('} },')
      s = append(s, paste('{ data: { id: "',i+e,'", ',
                          'source: "',  which(V(g)$name == get.edgelist(g)[e,1])  ,'", target: "',  which(V(g)$name == get.edgelist(g)[e,2])  ,'", ',
                          'dimension: "', edge.attrs[k],'", value:"',eval(parse(text=paste("E(g)$",edge.attrs[k],'[',e,']',sep=''))),'"',
                          '} },', sep='')
                 )
    }
  }
#   s = substr(s, 1, nchar(s)-1)
  s = append(s, ']')
#   s = gsub('},]', '}]', s)
#   return(paste(s, collapse="") )
#   return( gsub('},]', '}]', paste(s, collapse="")) )
  return( gsub("\"","'", gsub('},]', '}]', paste(s, collapse="")) ) )
}
tpyFunc(genemania.network.graph)


pubMedCounts <- function(graphObj) {
  vert_pubmedCount <- c()
  for (g in V(graphObj)$name) { 
    vert_pubmedCount <- cbind(vert_pubmedCount, entrez_search(db="pubmed", term=paste(g,'[All Fields] AND "humans"[MeSH Terms]',sep=''))$count )
  }
  return( sqrt(vert_pubmedCount/pi) )
}


library(rentrez)
entrez_search(db="pubmed", term='LIPC[All Fields] AND "humans"[MeSH Terms]')
vert_pubmedCount <- c()
for (g in V(genemania.network.graph)$name) { 
  cat(paste(g,'[All Fields] AND "humans"[MeSH Terms]',sep=''),'\n') 
  vert_pubmedCount <- cbind(vert_pubmedCount, entrez_search(db="pubmed", term=paste(g,'[All Fields] AND "humans"[MeSH Terms]',sep=''))$count )
}
# radius adjusted values
barplot(sqrt(vert_pubmedCount/pi), las=2, horiz=T, names.arg = V(genemania.network.graph)$name, cex.names = 0.6)


# MANOVA example
y1 = c(18.2, 20.1, 17.6, 16.8, 18.8, 19.7, 19.1)
y2 = c(17.4, 18.7, 19.1, 16.4, 15.9, 18.4, 17.7)
y3 = c(15.2, 18.8, 17.7, 16.5, 15.9, 17.1, 16.7)
y = c(y1, y2, y3)
n = rep(7, 3)
group = rep(1:3, n)
groupData = data.frame(y = y, group = factor(group))
fit = lm(y ~ group, groupData)
anova(fit)

# what to do to run F-test on net vs path data
chisq.test(matrix(as.integer(runif(52)*100), 4, 13), matrix(as.integer(runif(52)*100), 4, 13))

library(org.Hs.eg.db)
xx <- as.list(org.Hs.egALIAS2EG)
genenames <- names(xx)
# randomly sample 30 genes
sample(genenames, 10, replace=T)


