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
#genemania.network.graph <- remove.vertex.attribute(genemania.network.graph, "name")

tpyFunc <- function(g) {
  s = c();
  s = append(s, '[')
  iter = 0;
  vert.attrs <- list.vertex.attributes(g);
  edge.attrs <- list.edge.attributes(g);
  for (i in 1:length(V(g))) {
    iter = iter + 1; # changing i's to iter's
    s = append(s, paste('{ \"data\": { \"id\": "',iter,'", ', sep=''))
    for (k in 1:length(vert.attrs)) {
      if (vert.attrs[k] != "name") {
          s = append(s, paste('\"name\" : "', eval(parse(text=paste("V(g)$name",'[',iter,']',sep=''))),'", \"dimension\" : "',vert.attrs[k],'", \"value\" : "', eval(parse(text=paste("V(g)$",vert.attrs[k],'[',iter,']',sep=''))),'", ', sep='') )
      }
    }
   s = append(s, paste(' } },', sep=''))
  }

  for (e in 1:dim(unique(get.edgelist(g)))[1]) {
#     cat( e,
#          unique(get.edgelist(g))[e,1],
#          unique(get.edgelist(g))[e,2],
#          length(which(get.edgelist(g)[,1] == unique(get.edgelist(g))[e,1] & get.edgelist(g)[,2] == unique(get.edgelist(g))[e,2])),
#          mean(E(g)$weight[which(get.edgelist(g)[,1] == unique(get.edgelist(g))[e,1] & get.edgelist(g)[,2] == unique(get.edgelist(g))[e,2])]),
#          '\n')
    for (k in 1:length(edge.attrs)) {
      iter = iter + 1;
      if (  typeof(eval(parse(text=paste("E(g)$",edge.attrs[k],'[',e,']',sep='')))) == "character"  ) {
        cat('more to do','\n')
        edgeattrmean = as.numeric(eval(parse(text=paste("E(g)$",edge.attrs[k],'[',e,']',sep=''))))
        s = append(s, paste('{ \"data\": { \"id\": "',iter,'", ',
                            '\"source\": "',  which(V(g)$name == unique(get.edgelist(g))[e,1])  ,'", \"target\": "',  which(V(g)$name == unique(get.edgelist(g))[e,2])  ,'", ',
                            '\"dimension\": "', edge.attrs[k],'", \"value\":"',format(edgeattrmean, scientific=F),'"',
                            '} },', sep='') )
      }
      else {
        numerator = sum(as.numeric(E(g)$weight[which(get.edgelist(g)[,1] == unique(get.edgelist(g))[e,1] & get.edgelist(g)[,2] == unique(get.edgelist(g))[e,2])]))
        denominator = length(E(g)$weight[which(get.edgelist(g)[,1] == unique(get.edgelist(g))[e,1] & get.edgelist(g)[,2] == unique(get.edgelist(g))[e,2])])
        edgeattrmean = numerator / denominator
        #edgeattrmean = mean(E(g)$weight[which(get.edgelist(g)[,1] == unique(get.edgelist(g))[e,1] & get.edgelist(g)[,2] == unique(get.edgelist(g))[e,2])])
        s = append(s, paste('{ \"data\": { \"id\": "',iter,'", ',
                            '\"source\": "',  which(V(g)$name == unique(get.edgelist(g))[e,1])  ,'", \"target\": "',  which(V(g)$name == unique(get.edgelist(g))[e,2])  ,'", ',
                            '\"dimension\": "', edge.attrs[k],'", \"value\":"',format(edgeattrmean, scientific=F),'"',
                            '} },', sep='')
        )
      }
    }
  }
  s = append(s, ']')
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

findEdgesForVertex <- function(vertexname,gra) {
  return( which(vertexname == get.edgelist(gra)[,1] | vertexname == get.edgelist(gra)[,2]) )
}

E(genemania.network.graph)$weight[findEdgesForVertex(V(genemania.network.graph)$name[1], genemania.network.graph)]
unique(get.edgelist(genemania.network.graph))
which(get.edgelist(genemania.network.graph)[,1] == unique(get.edgelist(genemania.network.graph))[1,1] & get.edgelist(genemania.network.graph)[,2] == unique(get.edgelist(genemania.network.graph))[1,2])
E(genemania.network.graph)$weight[which(get.edgelist(genemania.network.graph)[,1] == unique(get.edgelist(genemania.network.graph))[1,1] & get.edgelist(genemania.network.graph)[,2] == unique(get.edgelist(genemania.network.graph))[1,2])]

# can use the which indexes to average duplicate entries.
for (u in 1:dim(unique(get.edgelist(genemania.network.graph)))[1]) {
  cat( u,
       unique(get.edgelist(genemania.network.graph))[u,1],
       unique(get.edgelist(genemania.network.graph))[u,2],
       length(which(get.edgelist(genemania.network.graph)[,1] == unique(get.edgelist(genemania.network.graph))[u,1] & get.edgelist(genemania.network.graph)[,2] == unique(get.edgelist(genemania.network.graph))[u,2])),
       mean(E(genemania.network.graph)$weight[which(get.edgelist(genemania.network.graph)[,1] == unique(get.edgelist(genemania.network.graph))[u,1] & get.edgelist(genemania.network.graph)[,2] == unique(get.edgelist(genemania.network.graph))[u,2])]),
       '\n')
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



# Mock data for evaluation
matrix(c(sample(0:255, 50, replace = T),sample(0:255, 50, replace = T),sample(0:255, 50, replace = T)), 50, 3) # RGB node colors
c(sample(0:15, 50, replace = T)) # border width
sample(c("rectangle", "roundrectangle", "ellipse", "triangle", "pentagon", "hexagon", "heptagon", "octagon", "star", "diamond", "vee", "rhomboid"), 50, replace = T) # shape
sample(1:300, 50, replace = T) # radius
sample(1:20, 50, replace = T) # edge width
matrix(c(sample(0:255, 50, replace = T),sample(0:255, 50, replace = T),sample(0:255, 50, replace = T)), 50, 3) # RGB line colors
sample(c("solid", "dotted", "dashed"), 50, replace = T) # line pattern

experimentData <- cbind(
      sample(c("Person1","Person2","Person3","Person4"), 4, replace = T),
      sample(c("Network1","Network2"), 2, replace = T),
      matrix(c(sample(0:255, 50, replace = T),sample(0:255, 50, replace = T),sample(0:255, 50, replace = T)), 50, 3), # RGB node colors
      c(sample(0:15, 50, replace = T)), # border width
      sample(c("rectangle", "roundrectangle", "ellipse", "triangle", "pentagon", "hexagon", "heptagon", "octagon", "star", "diamond", "vee", "rhomboid"), 50, replace = T), # shape
      sample(1:300, 50, replace = T), # radius
      sample(1:20, 50, replace = T), # edge width
      matrix(c(sample(0:255, 50, replace = T),sample(0:255, 50, replace = T),sample(0:255, 50, replace = T)), 50, 3), # RGB line colors
      sample(c("solid", "dotted", "dashed"), 50, replace = T), # line pattern
      sample(0:1, 50, replace = T) #selected or not
)

colnames(experimentData) <- c("Subject","Network","NodeR","NodeG","NodeB","NodeBorderWidth","NodeShape","NodeRadius","EdgeWidth","EdgeR","EdgeG","EdgeB","EdgePattern","Selected")
head(experimentData)
experimentData <- as.data.frame(experimentData)
lmer(as.numeric(Selected) ~ as.numeric(NodeRadius) + (1|NodeShape), data=experimentData)
experiment.model <- lmer(as.numeric(Selected) ~ as.numeric(NodeRadius) + (1|Subject) + (1|Network), data=experimentData)
experiment.model2 <- lmer(as.numeric(Selected) ~ as.numeric(NodeRadius) + NodeShape + (1|Subject) + (1|Network), data=experimentData)
experiment.null <- lmer(as.numeric(Selected) ~ (1|Subject) + (1|Network), data=experimentData)
anova(experiment.null,experiment.model)


# Selected ~ Node Encodings ... + ... Edge Encodings + ... + (1|Subject) + (1|Network)
# cols are node visual encodings from app, values are 1 or 0 depending on highlights
# for values that are value 1, can replace 1 with the actual value as obtained from collected data (e.g. color, radius, shape, etc)
# This could be useful in determing whether the encoding is noticeable, rather than of the value of the encoding itself.
# Network could refer to different FD layouts, presented in randomized order to prevent carry-over effects, but also account for layout random effects

experimentData <- cbind(
  sample(c("Person1","Person2","Person3","Person4"), 4, replace = T),
  sample(c("Network1","Network2"), 2, replace = T),
  sample(0:1, 50, replace = T),
  
)

expdat <- matrix(0,50,13)
for (i in 1:dim(expdat)[1]) {
  expdat[i,sample(1:7,1)] <- 1
  expdat[i,sample(8:13,1)] <- 1
}
expdat <- cbind(expdat, sample(0:1, 50, replace = T))
colnames(expdat) <- c("NodeColorSeq","NodeColorDiv","NodeColorCat","NodeBorderQuant","NodeBorderBin","NodeSizeQuant","NodeSizeBin",
                      "EdgeWidthQuant","EdgeWidthBin","EdgeColorSeq","EdgeColorDiv","EdgeColorCat","EdgePatternCat","UserSelected")

library(RJSONIO)
jdat <- fromJSON('~/Code/VisualEncodingEngine/jsserver/savedData/1469473953844.json')
jnode <- c()
for (l in 1:length(jdat)) {
  if(jdat[[l]]$type == "node") {
    jnode = append(jnode, c(jdat[[l]]$id, jdat[[l]]$name, jdat[[l]]$results, jdat[[l]]$positions, jdat[[l]]$selected))
  }
}
expd <- data.frame(t(matrix(jnode, 9, 23)))
colnames(expd) <- c("id", "name", "nodecolor", "nodeshape", "nodeborder", "nodesize", "xpos", "ypos", "selected")
expd[,5] <- as.numeric(gsub('px','',expd[,5]))
expd[,6] <- as.numeric(gsub('px','',expd[,6]))
expd[,7] <- as.numeric(as.character(expd[,7]))
expd[,8] <- as.numeric(as.character(expd[,8]))
expd[,9] <- as.numeric(as.character(expd[,9]))

loadResults <- function(path) {
  files = list.files(path)
  jnode <- c()
  for (f in files) {
    cat(paste(path,f,sep=''), '\n')
    jdat <- fromJSON(paste(path,f,sep=''))
    for (l in 1:length(jdat)) {
      if(jdat[[l]]$type == "node") {
        jnode = append(jnode, c(f, jdat[[l]]$id, jdat[[l]]$name, jdat[[l]]$encoding, jdat[[l]]$results, jdat[[l]]$positions, jdat[[l]]$selected))
      }
    }
  }
  return(jnode)
}

expd <- loadResults('~/Code/VisualEncodingEngine/jsserver/savedData/')
expd <- data.frame(t(matrix(expd, 11, length(expd)/11)))
colnames(expd) <- c("file","id", "name", "encoding", "nodecolor", "nodeshape", "nodeborder", "nodesize", "xpos", "ypos", "selected")
expd[,7] <- as.numeric(gsub('px','',expd[,7]))
expd[,8] <- as.numeric(gsub('px','',expd[,8]))
expd[,9] <- as.numeric(as.character(expd[,9]))
expd[,10] <- as.numeric(as.character(expd[,10]))
expd[,11] <- as.numeric(as.character(expd[,11]))
expd[sample(dim(expd)[1], 11, replace = F),11] <- 1

loadResultsDoubleEnc <- function(path) {
  files = list.files(path)
  jnode <- c()
  for (f in files) {
    cat(paste(path,f,sep=''), '\n')
    jdat <- fromJSON(paste(path,f,sep=''))
    for (l in 1:length(jdat)) {
      if(jdat[[l]]$type == "node") {
        #jnode = append(jnode, c(f, jdat[[l]]$id, jdat[[l]]$name, jdat[[l]]$encoding1, jdat[[l]]$encoding2, jdat[[l]]$results, jdat[[l]]$positions, jdat[[l]]$selected))
        jnode = append(jnode, c(f, jdat[[l]]$id, jdat[[l]]$name, jdat[[l]]$encoding1, jdat[[l]]$encoding2, jdat[[l]]$results, jdat[[l]]$positions, jdat[[l]]$selected, jdat[[l]]$clickX, jdat[[l]]$clickY, jdat[[l]]$windowheight, jdat[[l]]$windowwidth, jdat[[l]]$browser, jdat[[l]]$group))
      }
    }
  }
  return(jnode)
}

expd <- loadResultsDoubleEnc('~/Code/VisualEncodingEngine/jsserver/savedData/')
# expd <- data.frame(t(matrix(expd, 12, length(expd)/12)))
expd <- data.frame(t(matrix(expd, 18, length(expd)/18))) 
# colnames(expd) <- c("file","id", "name", "encoding1", "encoding2", "nodecolor", "nodeshape", "nodeborder", "nodesize", "xpos", "ypos", "selected")
colnames(expd) <- c("file","id", "name", "encoding1", "encoding2", "nodecolor", "nodeshape", "nodeborder", "nodesize", "xpos", "ypos", "selected", "clickX", "clickY", "windowHeight", "windowWidth", "Browser", "Group")
expd[,8] <- as.numeric(gsub('px','',expd[,8]))
expd[,9] <- as.numeric(gsub('px','',expd[,9]))
expd[,10] <- as.numeric(as.character(expd[,10]))
expd[,11] <- as.numeric(as.character(expd[,11]))
expd[,12] <- as.numeric(as.character(expd[,12]))

expd$nodecolor <- revalue(expd$nodecolor, c("#999"="#999999"))


clickmap <- function() {
  #plot(as.numeric(as.character(expd$clickX)) / as.numeric(as.character(expd$windowWidth)), as.numeric(as.character(expd$clickY)) / as.numeric(as.character(expd$windowHeight)))
  plot(as.numeric(as.character(expd$xpos[which(expd$selected == 0)])) / as.numeric(as.character(expd$windowWidth[which(expd$selected == 0)])), as.numeric(as.character(expd$ypos[which(expd$selected == 0)])) / as.numeric(as.character(expd$windowHeight[which(expd$selected == 0)])), col="gray",
       xlab="Normalized X Coordinate Position",
       ylab="Normalized Y Coordinate Position",
       main="Click Map of Selected Nodes Versus Unselected")
  points(as.numeric(as.character(expd$xpos[which(expd$selected == 1)])) / as.numeric(as.character(expd$windowWidth[which(expd$selected == 1)])), as.numeric(as.character(expd$ypos[which(expd$selected == 1)])) / as.numeric(as.character(expd$windowHeight[which(expd$selected == 1)])), col="red", pch=2)
}

# Basic Visualizations
clickmap()

barplot(table(expd[which(expd$selected == 1),3]), horiz = T, las=1)
barplot(table(paste(expd[which(expd$selected == 1),4],expd[which(expd$selected == 1),5])), horiz=T, las=1)
tpch <- rep(1,dim(expd)[1])
tpch[which(expd.mod$selected == 1)] <- 1
ts <- rep(1,dim(expd)[1])
ts[which(expd.mod$selected == 1)] <- 1
pairs(expd.mod[,2:4], col=as.character(expd$nodecolor), pch=tpch, cex=ts)
pairs(expd.mod[,2:4], col=ifelse(expd.mod$selected == 1, as.character(expd$nodecolor), "gray"), pch=tpch, cex=ts)


#euclidian distance from center
for (f in levels(expd[,1])) { 
  cat(f,'\n')
  #expd[which(expd[,1] == f),12] <- calcDistanceFromCenterOfNetwork(f)
  expd[which(expd[,1] == f),13] <- calcDistanceFromCenterOfNetworkDoubleEnc(f)
}

calcDistanceFromCenterOfNetworkDoubleEnc <- function(file) {
  return(  c(expd[which(expd[,1] == file),10] - mean(expd[which(expd[,1] == file),11]) + expd[which(expd[,1] == file),10] - mean(expd[which(expd[,1] == file),11]))^2  )
}


# Add centrality data to data frame
expdwcent <- expd
expdwcent <- data.frame(cbind(expdwcent,
                   rep(centralization.betweenness(genemania.network.graph)$res, dim(expd)[1]),
                   rep(centralization.closeness(genemania.network.graph)$res, dim(expd)[1]),
                   rep(centralization.degree(genemania.network.graph)$res, dim(expd)[1]),
                   rep(centralization.evcent(genemania.network.graph)$vector, dim(expd)[1])
))
colnames(expdwcent) <- c("file","id", "name", "encoding1", "encoding2", "nodecolor", "nodeshape", "nodeborder", "nodesize", "xpos", "ypos", "selected", "clickX", "clickY", "windowHeight", "windowWidth", "betweenness", "closeness", "degree", "eigenvector")


colnames(expd) <- c("file","id", "name", "encoding1", "encoding2", "nodecolor", "nodeshape", "nodeborder", "nodesize", "xpos", "ypos", "selected","distCent")
expd[,13] <- as.numeric(as.character(expd[,13]))
expd <- as.data.frame(cbind(expd[,1:11],expd[,13],expd[,12]))
colnames(expd) <- c(colnames(expd)[1:11],"distCent","selected")

library(lme4)
expd.model1 <- lmer(as.numeric(selected) ~ as.numeric(nodeborder) + (1|name) + (1|file), data=expd)
expd.model2 <- lmer(as.numeric(selected) ~ as.numeric(nodesize) + (1|name) + (1|file), data=expd)
anova(expd.model1,expd.model2)

summary(expd.model1)
coef(expd.model1)

summary(lm(as.numeric(selected) ~ as.numeric(nodeborder) + log10(distCent) + as.numeric(nodesize) + name, data=expd))


mod.coef <- coef(lmer(as.numeric(selected) ~ as.numeric(nodeborder) + as.numeric(nodesize) + nodecolor + as.numeric(xpos) + as.numeric(ypos) + (1|name) + (1|file) + (1|encoding1) + (1|encoding2), data=expd))
mod.coef$name
heatmap(as.matrix(mod.coef$name), margins = c(10,10))


# randomly sampling an equal number rows of zero and one selection values
rsexpd <- expd[c(c(sample(which(expd[,11] == 0), length(which(expd[,11] == 1)))),c(which(expd[,11] == 1))),]
coef(lmer(as.numeric(selected) ~ as.numeric(nodesize) + (1|name) + (1|file), data=rsexpd))
coef(lmer(as.numeric(selected) ~ as.numeric(nodesize) + (1|name) + (1|encoding), data=rsexpd))
summary(lmer(as.numeric(selected) ~ as.numeric(nodesize) + as.numeric(nodeborder) + log10(distCent) + (1|name) + (1|encoding), data=rsexpd))
coef(lmer(as.numeric(selected) ~ as.numeric(nodesize) + as.numeric(nodeborder) + log10(distCent) + (1|name) + (1|encoding), data=rsexpd))


# Let's use an RF
library(randomForest)
expd.mod <- expd[,c(1,3,6:12)] #until 13 if I want to include distCent
#rf1 <- randomForest(as.numeric(selected) ~ ., data=expd, importance=TRUE, proximity=TRUE)
rf1 <- randomForest(as.numeric(selected) ~ ., data=expd.mod, importance=TRUE, proximity=TRUE)
print(rf1)
rf1$importance
varImpPlot(rf1,type=2)


voodoo <- c()
for (i in 1:dim(expd)[1]) {
  if (expd[i,6] == "#999") {
    voodoo <- append(voodoo, t(col2rgb("#999999")))
  }
  else {
    voodoo <- append(voodoo, t(col2rgb(expd[i,6])))
  }
}
mycolors <- t(matrix(voodoo, 3, length(voodoo)/3))
#r, g, b cols

expd.mod <- data.frame(cbind(expd[,3],mycolors[,1:3],expd[,7:18])) #until 13 if I want distCent
colnames(expd.mod) <- c("name", "R", "G", "B", colnames(expd)[7:18])
rf2 <- randomForest(as.numeric(selected) ~ ., data=expd.mod, importance=TRUE, proximity=TRUE, do.trace = TRUE)
print(rf2)
rf2$importance
varImpPlot(rf2,type=2)

# unsupervised
expd.urf <- randomForest(expd.mod[, -11])
MDSplot(expd.urf, expd$selected)

#regression
predict(rf2, expd.mod[sample(which(expd.mod[,11] == 1), 1),-11])
predict(rf2, expd.mod[sample(which(expd.mod[,11] == 0), 1),-11])

plot(rf2$predicted)

# optimizing mtry
tuneRF(x = expd.mod[,-11], y = expd.mod[,11], plot = T, doBest = T)


#trying to balance classes for RF
expd.mod.bal <- expd.mod[c(c(sample(which(expd.mod[,11] == 0), length(which(expd.mod[,11] == 1)))),c(which(expd.mod[,11] == 1))),]
tuneRF(x = expd.mod.bal[,-11], y = expd.mod.bal[,11], plot = T, doBest = T)
rf3 <- randomForest(as.numeric(selected) ~ ., data=expd.mod.bal, importance=TRUE, proximity=TRUE, do.trace = F, mtry=2)
print(rf3)
rf3$importance
varImpPlot(rf3,type=2)
rf4 <- randomForest(selected ~ ., data=expd.mod, importance=TRUE, proximity=TRUE, do.trace = F, mtry=2, strata = selected, sampsize = sum(expd.mod[,11] == 1))
print(rf4)
rf4$importance
varImpPlot(rf3,type=2)

#compare balanced vs unbalanced
library(ROCR)

rf3.perf = performance(  prediction(labels = expd.mod.bal$selected, predictions = rf3$predicted)  ,"tpr","fpr")
rf4.perf = performance(  prediction(labels = expd.mod$selected, predictions = rf4$predicted)  ,"tpr","fpr")

#plot the curve
plot(rf4.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
lines(unlist(rf3.perf@x.values),unlist(rf3.perf@y.values), col=4, lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#compute area under curve

auc.rf3 <- performance(    prediction(labels = expd.mod.bal$selected, predictions = rf3$predicted)   ,"auc")
auc.rf4 <- performance(    prediction(labels = expd.mod$selected, predictions = rf4$predicted)   ,"auc")

auc.rf3 <- unlist(slot(auc.rf3, "y.values"))
auc.rf4 <- unlist(slot(auc.rf4, "y.values"))

minauc<-min(round(auc.rf3, digits = 2))
maxauc<-max(round(auc.rf3, digits = 2))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
minauct
maxauct

minauc<-min(round(auc.rf4, digits = 2))
maxauc<-max(round(auc.rf4, digits = 2))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
minauct
maxauct







# Aim 2 paper

aim2d <- matrix(rbind(
  c(2,	5),
  c(30,	25),
  c(8,	3),
  c(3,	2),
  c(5,	1),
  c(27,	11),
  c(12,	15),
  c(9,	14)
), 8, 2)
colnames(aim2d) <- c("Network",  "Pathway")
rownames(aim2d) <- c("Position",
                     "Text",
                     "Size,Area",
                     "Weight, Boldness",
                     "Saturation, Brightness",
                     "Color",
                     "Shape, Icon",
                     "Enclosure, Connection")

fisher.test(aim2d)
#prop.test(aim2d, conf.level = 0.95, correct = TRUE)

aim2de <- matrix(t(rbind(
c(2,0,0,0,14,0,4,4,2,3),
c(8,18,1,1,11,2,13,9,4,5))), 10, 2)
colnames(aim2de) <- c("Network",  "Pathway")
rownames(aim2de) <- c("Position",
                      "Text",
                      "Size,Area",
                      "Saturation, Brightness",
                      "Color",
                      "Shape, Icon",
                      "Enclosure, Connection",
                      "Line Pattern",
                      "Line Endings",
                      "Line Weight")
aim2de <- aim2de[c(1,2,5,7,8,9,10),] #removing inappropriate values (zero values and encodings that don't make sense)

fisher.test(aim2de)
chisq.test(aim2de)

var.test(aim2d[,1],aim2d[,2])
var.test(aim2de[,1],aim2de[,2])




# Comparing centarlities to user selection
centrality_vals <- cbind(centralization.closeness(genemania.network.graph)$res, centralization.betweenness(genemania.network.graph)$res, centralization.degree(genemania.network.graph)$res, centralization.evcent(genemania.network.graph)$vector, coef(lmer(as.numeric(selected) ~ as.numeric(nodesize) + as.numeric(nodeborder) + log10(distCent) + (1|name) + (1|encoding), data=rsexpd))$name[,1])
colnames(centrality_vals) <- c("closeness", "betweenness", "degree", "eigenvector", "lmercoef")
rownames(centrality_vals) <- V(genemania.network.graph)$name
cor(centrality_vals)
plot(centrality_vals[,1]/max(centrality_vals[,1]), centrality_vals[,5], pch = 1, col=c("black"), xlim=c(0,1), ylim=c(0,1))
points(centrality_vals[,2]/max(centrality_vals[,2]), centrality_vals[,5], pch = 2, col=c("red"))
points(centrality_vals[,3]/max(centrality_vals[,3]), centrality_vals[,5], pch = 3, col=c("blue"))
points(centrality_vals[,4]/max(centrality_vals[,4]), centrality_vals[,5], pch = 4, col=c("purple"))
# little correlation between any centrality measurement and coefficients from model. Thus, currently centrality methods may not ...


# Graph density exploration
rg1 <- random.graph.game(10, 0.05)
rg2 <- random.graph.game(50, 0.05)
rg3 <- random.graph.game(100, 0.05)
rg1 <- random.graph.game(10, 0.5)
rg2 <- random.graph.game(50, 0.5)
rg3 <- random.graph.game(100, 0.5)

hist(centralization.betweenness(rg1, directed = FALSE, normalized = TRUE)$res)
hist(centralization.closeness(rg1, normalized = TRUE)$res)
hist(centralization.degree(rg1, normalized = TRUE)$res)
hist(centralization.evcent(rg1, normalized = TRUE)$vector)
plot(rg1, vertex.size=8, vertex.label=NA)

BlueCols100 <- colorRampPalette(c(brewer.pal(9, "Blues")[1],brewer.pal(9, "Blues")[9]))(100)
RedCols100 <- colorRampPalette(c(brewer.pal(9, "Reds")[1],brewer.pal(9, "Reds")[9]))(100)
par(mfrow=c(2,2))
plot(rg1, vertex.size=8, vertex.label=NA, vertex.color=BlueCols100[rank(centralization.betweenness(rg1, directed = FALSE, normalized = TRUE)$res)])
plot(rg1, vertex.size=8, vertex.label=NA, vertex.color=BlueCols100[rank(centralization.closeness(rg1, normalized = TRUE)$res)])
plot(rg1, vertex.size=8, vertex.label=NA, vertex.color=BlueCols100[rank(centralization.degree(rg1, normalized = TRUE)$res)])
plot(rg1, vertex.size=8, vertex.label=NA, vertex.color=BlueCols100[rank(centralization.evcent(rg1, normalized = TRUE)$vector)])
dev.off()

par(mfrow=c(2,4))
hist(centralization.betweenness(rg1, directed = FALSE, normalized = TRUE)$res, main="Betweeness", xlab=NA)
hist(centralization.closeness(rg1, normalized = TRUE)$res, main="Closeness", xlab=NA)
hist(centralization.degree(rg1, normalized = TRUE)$res, main="Degree", xlab=NA)
hist(centralization.evcent(rg1, normalized = TRUE)$vector, main="Eigenvector", xlab=NA)
plot(rg1, vertex.size=8, vertex.label=NA, vertex.color=BlueCols100[rank(centralization.betweenness(rg1, directed = FALSE, normalized = TRUE)$res)])
plot(rg1, vertex.size=8, vertex.label=NA, vertex.color=BlueCols100[rank(centralization.closeness(rg1, normalized = TRUE)$res)])
plot(rg1, vertex.size=8, vertex.label=NA, vertex.color=BlueCols100[rank(centralization.degree(rg1, normalized = TRUE)$res)])
plot(rg1, vertex.size=8, vertex.label=NA, vertex.color=BlueCols100[rank(centralization.evcent(rg1, normalized = TRUE)$vector)])
dev.off()

plot_histogramXthreeSizes<- function() {
  par(mfrow=c(3,4))
  hist(centralization.betweenness(rg1, directed = FALSE, normalized = TRUE)$res, main="Betweeness", xlab=NA)
  hist(centralization.closeness(rg1, normalized = TRUE)$res, main="Closeness", xlab=NA)
  hist(centralization.degree(rg1, normalized = TRUE)$res, main="Degree", xlab=NA)
  hist(centralization.evcent(rg1, normalized = TRUE)$vector, main="Eigenvector", xlab=NA)
  hist(centralization.betweenness(rg2, directed = FALSE, normalized = TRUE)$res, main="Betweeness", xlab=NA)
  hist(centralization.closeness(rg2, normalized = TRUE)$res, main="Closeness", xlab=NA)
  hist(centralization.degree(rg2, normalized = TRUE)$res, main="Degree", xlab=NA)
  hist(centralization.evcent(rg2, normalized = TRUE)$vector, main="Eigenvector", xlab=NA)
  hist(centralization.betweenness(rg3, directed = FALSE, normalized = TRUE)$res, main="Betweeness", xlab=NA)
  hist(centralization.closeness(rg3, normalized = TRUE)$res, main="Closeness", xlab=NA)
  hist(centralization.degree(rg3, normalized = TRUE)$res, main="Degree", xlab=NA)
  hist(centralization.evcent(rg3, normalized = TRUE)$vector, main="Eigenvector", xlab=NA)
}

plot_centralitiesXthreeSizes<- function() {
  par(mfrow=c(3,4))
  BlueCols <- colorRampPalette(c(brewer.pal(9, "Blues")[1],brewer.pal(9, "Blues")[9]))(length(V(rg1)))
  plot(rg1, vertex.size=8, vertex.label=NA, vertex.color=BlueCols[rank(centralization.betweenness(rg1, directed = FALSE, normalized = TRUE)$res)])
  plot(rg1, vertex.size=8, vertex.label=NA, vertex.color=BlueCols[rank(centralization.closeness(rg1, normalized = TRUE)$res)])
  plot(rg1, vertex.size=8, vertex.label=NA, vertex.color=BlueCols[rank(centralization.degree(rg1, normalized = TRUE)$res)])
  plot(rg1, vertex.size=8, vertex.label=NA, vertex.color=BlueCols[rank(centralization.evcent(rg1, normalized = TRUE)$vector)])
  BlueCols <- colorRampPalette(c(brewer.pal(9, "Blues")[1],brewer.pal(9, "Blues")[9]))(length(V(rg2)))
  plot(rg2, vertex.size=8, vertex.label=NA, vertex.color=BlueCols[rank(centralization.betweenness(rg2, directed = FALSE, normalized = TRUE)$res)])
  plot(rg2, vertex.size=8, vertex.label=NA, vertex.color=BlueCols[rank(centralization.closeness(rg2, normalized = TRUE)$res)])
  plot(rg2, vertex.size=8, vertex.label=NA, vertex.color=BlueCols[rank(centralization.degree(rg2, normalized = TRUE)$res)])
  plot(rg2, vertex.size=8, vertex.label=NA, vertex.color=BlueCols[rank(centralization.evcent(rg2, normalized = TRUE)$vector)])
  BlueCols <- colorRampPalette(c(brewer.pal(9, "Blues")[1],brewer.pal(9, "Blues")[9]))(length(V(rg3)))
  plot(rg3, vertex.size=8, vertex.label=NA, vertex.color=BlueCols[rank(centralization.betweenness(rg3, directed = FALSE, normalized = TRUE)$res)])
  plot(rg3, vertex.size=8, vertex.label=NA, vertex.color=BlueCols[rank(centralization.closeness(rg3, normalized = TRUE)$res)])
  plot(rg3, vertex.size=8, vertex.label=NA, vertex.color=BlueCols[rank(centralization.degree(rg3, normalized = TRUE)$res)])
  plot(rg3, vertex.size=8, vertex.label=NA, vertex.color=BlueCols[rank(centralization.evcent(rg3, normalized = TRUE)$vector)])
}

plot_histogramXthreeSizes()
plot_centralitiesXthreeSizes()



# Graph density exploration
rg1 <- random.graph.game(10, 0.05)
rg2 <- random.graph.game(50, 0.05)
rg3 <- random.graph.game(100, 0.05)
rg4 <- random.graph.game(10, 0.1)
rg5 <- random.graph.game(50, 0.1)
rg6 <- random.graph.game(100, 0.1)
rg7 <- random.graph.game(10, 0.15)
rg8 <- random.graph.game(50, 0.15)
rg9 <- random.graph.game(100, 0.15)
par(mfrow=c(3,3))
plot(rg1, vertex.size=8, vertex.label=NA)
plot(rg2, vertex.size=8, vertex.label=NA)
plot(rg3, vertex.size=8, vertex.label=NA)
plot(rg4, vertex.size=8, vertex.label=NA)
plot(rg5, vertex.size=8, vertex.label=NA)
plot(rg6, vertex.size=8, vertex.label=NA)
plot(rg7, vertex.size=8, vertex.label=NA)
plot(rg8, vertex.size=8, vertex.label=NA)
plot(rg9, vertex.size=8, vertex.label=NA)
dev.off()


# Graph density exploration
plot_densityXsize <- function(dens,size, mar1, mar2) {
  if (missing(mar1)) {
    mar1 <- length(dens)
  }
  if (missing(mar2)) {
    mar2 <- length(size)
  }
  numer = 1;
  par(mfrow=c(mar1, mar2))
  for (d in dens) {
    for (s in size) {
      cat(paste("rg",numer," <- random.graph.game(",s,",",d,")",sep=''), '\n')
      assign(paste("rg",numer,sep=''),eval(parse(text=paste("random.graph.game(",s,",",d,")",sep=''))), envir = .GlobalEnv)
      plot(random.graph.game(s, d), vertex.size=8, vertex.label=NA, vertex.color="black")
      numer = numer + 1;
    }
  }
}

plot_hist <- function(dens,size,func,title) {
  numer = 1;
  par(mfrow=c(length(dens), length(size)))
  for (d in dens) {
    for (s in size) { 
      if (title == "eigenvec") {
        hist(func(eval(parse(text=paste("rg",numer,sep=''))), normalized = TRUE)$vector, main=paste(title,"\ns:",s," d:",d,sep=''), xlab=NA)
        numer = numer + 1;
      }
      else {
        hist(func(eval(parse(text=paste("rg",numer,sep=''))), normalized = TRUE)$res, main=paste(title,"\ns:",s," d:",d,sep=''), xlab=NA)
        numer = numer + 1;
      }
    }
  }
}

pdf(file="/Users/nikhilgopal/Code/graph_sizing/p_vs_size.pdf")
plot_densityXsize(c(0.01, 0.05, 0.1, 0.15), c(10,25,50,75,100))
plot_hist(c(0.01, 0.05, 0.1, 0.15), c(10,25,50,75,100), centralization.betweenness, "betweenness")
plot_hist(c(0.01, 0.05, 0.1, 0.15), c(10,25,50,75,100), centralization.closeness, "closeness")
plot_hist(c(0.01, 0.05, 0.1, 0.15), c(10,25,50,75,100), centralization.degree, "degree")
plot_hist(c(0.01, 0.05, 0.1, 0.15), c(10,25,50,75,100), centralization.evcent, "eigenvec")
dev.off()


#pdf(file="/Users/nikhilgopal/Code/graph_sizing/brca_net.pdf")
pdf(file="/Users/nikhilgopal/Code/graph_sizing/app_net.pdf")
BlueCols100 <- colorRampPalette(c(brewer.pal(9, "Blues")[1],brewer.pal(9, "Blues")[9]))(length(V(genemania.network.graph)))
genemania.network.graph.layout <- layout.spring(genemania.network.graph)
par(mfrow=c(2,4))
hist(centralization.betweenness(genemania.network.graph, directed = FALSE, normalized = TRUE)$res, main="Betweeness", xlab=NA)
hist(centralization.closeness(genemania.network.graph, normalized = TRUE)$res, main="Closeness", xlab=NA)
hist(centralization.degree(genemania.network.graph, normalized = TRUE)$res, main="Degree", xlab=NA)
hist(centralization.evcent(genemania.network.graph, normalized = TRUE)$vector, main="Eigenvector", xlab=NA)
plot(genemania.network.graph, vertex.size=rank(centralization.betweenness(genemania.network.graph, directed = FALSE, normalized = TRUE)$res), 
     vertex.label=NA, vertex.color=BlueCols100[rank(centralization.betweenness(genemania.network.graph, directed = FALSE, normalized = TRUE)$res)],
     layout = genemania.network.graph.layout)
plot(genemania.network.graph, vertex.size=rank(centralization.closeness(genemania.network.graph, normalized = TRUE)$res), 
     vertex.label=NA, vertex.color=BlueCols100[rank(centralization.closeness(genemania.network.graph, normalized = TRUE)$res)],
     layout = genemania.network.graph.layout)
plot(genemania.network.graph, vertex.size=rank(centralization.degree(genemania.network.graph, normalized = TRUE)$res), 
     vertex.label=NA, vertex.color=BlueCols100[rank(centralization.degree(genemania.network.graph, normalized = TRUE)$res)],
     layout = genemania.network.graph.layout)
plot(genemania.network.graph, vertex.size=rank(centralization.evcent(genemania.network.graph, normalized = TRUE)$vector), 
     vertex.label=NA, vertex.color=BlueCols100[rank(centralization.evcent(genemania.network.graph, normalized = TRUE)$vector)],
     layout = genemania.network.graph.layout)

gmng.df <- data.frame(cbind(rank(centralization.betweenness(genemania.network.graph, directed = FALSE, normalized = TRUE)$res),
                 rank(centralization.closeness(genemania.network.graph, normalized = TRUE)$res),
                 rank(centralization.degree(genemania.network.graph, normalized = TRUE)$res),
                 rank(centralization.evcent(genemania.network.graph, normalized = TRUE)$vector)))
rownames(gmng.df) <- V(genemania.network.graph)$name
colnames(gmng.df) <- c("Betweenness", "Closeness", "Degree", "Eigenvector")
heatmap(as.matrix(gmng.df), Colv=as.dendrogram(hclust(dist(cor(gmng.df)))), Rowv=as.dendrogram(hclust(dist(gmng.df))), margins = c(13,5),
        col = BlueCols100)
dev.off()


rm(list=ls()[grep("rg\\d+", ls())])
plot_densityXsize(c(0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3), c(length(V(genemania.network.graph))), 3, 3)


compare_net_to_models <- function(func) {  
  par(mfrow=c(2,4))
  hist(func(genemania.network.graph, normalized = TRUE)$res, main="Custom", xlab=NA)
  hist(func(rg1, normalized = TRUE)$res, main="rg1", xlab=NA)
  hist(func(rg2, normalized = TRUE)$res, main="rg2", xlab=NA)
  hist(func(rg3, normalized = TRUE)$res, main="rg3", xlab=NA)
  hist(func(rg4, normalized = TRUE)$res, main="rg4", xlab=NA)
  hist(func(rg5, normalized = TRUE)$res, main="rg5", xlab=NA)
  hist(func(rg6, normalized = TRUE)$res, main="rg6", xlab=NA)
  hist(func(rg7, normalized = TRUE)$res, main="rg7", xlab=NA)
}


compare_net_to_models(centralization.closeness)

compare_models <- function(func, custom, modnum) {
  records <- c()
  for (i in 1:modnum) {
    tt <- wilcox.test(func(custom, normalized = TRUE)$res, func(eval(parse(text=paste("rg",i,sep=''))), normalized = TRUE)$res, paired = FALSE)
    records <- append(records, c(i, tt$p.value))
  }
  return(records)
}

compare_models(centralization.closeness, genemania.network.graph, 7)
compare_models(centralization.degree, genemania.network.graph, 7)
compare_models(centralization.betweenness, genemania.network.graph, 7)

# Sampling nodes from a network weighted by degree centrality of a given model network
k <- hist(centralization.degree(genemania.network.graph, normalized = T)$res, freq = FALSE)
sample(1:length(rep(k$density*10, k$counts)), 5, replace = F, prob = rep(k$density*10, k$counts))

V(genemania.network.graph)[sample(1:length(rep(k$density*10, k$counts)), 5, replace = F, prob = rep(k$density*10, k$counts))]

mdeg <- hist(centralization.degree(genemania.network.graph, normalized = T)$res, freq = FALSE)
mclo <- hist(centralization.closeness(genemania.network.graph, normalized = T)$res, freq = FALSE)
meig <- hist(centralization.evcent(genemania.network.graph, normalized = T)$vector, freq = FALSE)
mbet <- hist(centralization.betweenness(genemania.network.graph, normalized = T)$res, freq = FALSE)

mdeg$density*mclo$density*meig$density*mbet$density
barplot(c(
  sample(1:length(rep(mdeg$density*10, mdeg$counts)), 5, replace = F, prob = rep(mdeg$density*10, mdeg$counts)),
  sample(1:length(rep(mclo$density*10, mclo$counts)), 5, replace = F, prob = rep(mclo$density*10, mclo$counts)),
  sample(1:length(rep(meig$density*10, meig$counts)), 5, replace = F, prob = rep(meig$density*10, meig$counts)),
  sample(1:length(rep(mbet$density*10, mbet$counts)), 5, replace = F, prob = rep(mbet$density*10, mbet$counts))
))


# Experimental design thoughts
library(combinat)
combn(c("node color", "node shape", "node border", "node size"), m=2)
combn(c('node color (seq)', 
        'node color (div)',
        'node color (cat)',
        'node shape (cat)',
        'node border (quant)',
        'node border (bin)',
        'node size (quant)',
        'node size (bin)'), m=2)

# Manually counted 24 combinations to try if measuring every combination
# for nodes.

combn(c('edge width (quant)',
'edge width (bin)',
'edge color (seq)',
'edge color (div)',
'edge color (cat)',
'edge pattern (cat)',
'edge arrow (cat)'), m=2)

# Manually counted 11 combinations to try if measuring every combination
# for edges

# This makes 35 observations in total for users.

combn(c('node color (seq)', 
        'node color (div)',
        'node color (cat)',
        'node shape (cat)',
        'node border (quant)',
        'node border (bin)',
        'node size (quant)',
        'node size (bin)',
        'edge width (quant)',
        'edge width (bin)',
        'edge color (seq)',
        'edge color (div)',
        'edge color (cat)',
        'edge pattern (cat)',
        'edge arrow (cat)'), m=4)

combn(c('node color (seq)', 
        'node shape (cat)',
        'node border (quant)',
        'node size (quant)',
        'edge width (quant)',
        'edge color (seq)',
        'edge pattern (cat)'), m=4)







samplingMarkov <- function(options, weights) {
  return( sample(options, size = 1, prob = weights) )
}

sampled <- c()
for (i in 1:35) {
  sampled <- append(sampled, samplingMarkov(c("B->B","B->R","R->B","R->R"), c(0.25,0.25,0.25,0.25)))
}
table(sampled)


checkAdd <- function(options, weights) {
  samped <- sample(options, size = 1, prob = weights)
  if(samped == "B->B") { return( c(1,0,0,0) ) }
  else if (samped == "B->R") { return( c(0,1,0,0) ) }
  else if (samped == "R->B") { return( c(0,0,1,0) ) }
  else if (samped == "R->R") { return( c(0,0,0,1) )}
  else {
    cat('ERROR\n')
  }
}

sampledTab <- rbind(sampledTab, checkAdd(c("B->B","B->R","R->B","R->R"), c(0.25,0.25,0.25,0.25)))
#sampledTab <- sampledTab[2,]
apply(sampledTab, FUN=mean, MARGIN=2)
1-apply(sampledTab, FUN=mean, MARGIN=2)

sampledPlot <- c()
sampledTab <- matrix(0,1,4)
colnames(sampledTab) <- c("B->B","B->R","R->B","R->R")
sampledTab <- rbind(sampledTab, checkAdd(c("B->B","B->R","R->B","R->R"), c(0.25,0.25,0.25,0.25)))
#sampledTab <- sampledTab[2,]
for (i in 1:35) {
  newWeights <- 1-apply(sampledTab, FUN=mean, MARGIN=2)
  sampledTab <- rbind(sampledTab, checkAdd(c("B->B","B->R","R->B","R->R"), newWeights ))
  sampledPlot <- rbind(sampledPlot,c(i,newWeights))
}

plot(sampledPlot[,1], sampledPlot[,2], type = "l", col="purple",
     xlab="iteration", ylab="weight", main="Markov Chain Sampling")
lines(sampledPlot[,1], sampledPlot[,3], col="blue")
lines(sampledPlot[,1], sampledPlot[,4], col="green")
lines(sampledPlot[,1], sampledPlot[,5], col="red")

1-table(sampled)/sum(table(sampled))
1-apply(sampledTab, FUN=mean, MARGIN=2)

table(sampled)/sum(table(sampled))
apply(sampledTab, FUN=mean, MARGIN=2)



sampledPlot <- c()
sampledTab <- matrix(0,1,24)
colnames(sampledTab) <- c(1:24)
sampledTab <- rbind(sampledTab, checkAdd(c(1:24), rep(1/24, 24)))
#sampledTab <- sampledTab[2,]
for (i in 1:(24*100)) {
  newWeights <- 1-apply(sampledTab, FUN=mean, MARGIN=2)
  sampedVec <- matrix(0,1,24)
  sampedVec[samplingMarkov(c(1:24), newWeights)] <- 1
  sampledTab <- rbind(sampledTab, sampedVec)
  sampledPlot <- rbind(sampledPlot,c(i,newWeights))
}

apply(sampledTab, FUN=sum, MARGIN=2)
barplot(apply(sampledTab, FUN=sum, MARGIN=2))

sampled <- c()
for (i in 1:(24*100)) {
  sampled <- append(sampled, samplingMarkov(c(1:24), rep(1/24, 24)))
}
table(sampled)
barplot(table(sampled))


# Picking a network of the correct size

plot(induced.subgraph(genemania.network.graph, sample(V(genemania.network.graph)$name, 3)))
genemania.network.graph.randwalk <- induced.subgraph(genemania.network.graph,  random_walk(graph = genemania.network.graph, start = c(1), steps = c(4), mode = "all", stuck = "return")$name)
plot(genemania.network.graph.randwalk)
random_walk(graph = genemania.network.graph, start = c(1), steps = c(4), mode = "all", stuck = "return")

# Using the weight value below of 0.06 seems promising
plot(genemania.network.graph.randwalk, edge.color = ifelse(E(genemania.network.graph.randwalk)$weight >= 0.06, "red", NA))

genemania.network.graph.randwalk <- induced.subgraph(genemania.network.graph,  random_walk(graph = genemania.network.graph, start = c(1), steps = c(4), mode = "all", stuck = "return")$name)
tpyFunc(genemania.network.graph.randwalk)
genemania.network.graph.randwalk2 <- induced.subgraph(genemania.network.graph,  random_walk(graph = genemania.network.graph, start = c(1), steps = c(7), mode = "all", stuck = "return")$name)
plot(genemania.network.graph.randwalk2)
tpyFunc(genemania.network.graph.randwalk2)
genemania.network.graph.randwalk3 <- induced.subgraph(genemania.network.graph,  random_walk(graph = genemania.network.graph, start = c(1), steps = c(3), mode = "all", stuck = "return")$name)
tpyFunc(genemania.network.graph.randwalk3)

# Distribution of % of edges by weight value
# > sum(E(genemania.network.graph.randwalk)$weight >= 0.01)/sum(E(genemania.network.graph.randwalk)$weight >= 0)
# [1] 0.7142857
# > sum(E(genemania.network.graph.randwalk)$weight >= 0.02)/sum(E(genemania.network.graph.randwalk)$weight >= 0)
# [1] 0.5
# > sum(E(genemania.network.graph.randwalk)$weight >= 0.03)/sum(E(genemania.network.graph.randwalk)$weight >= 0)
# [1] 0.3214286
# > sum(E(genemania.network.graph.randwalk)$weight >= 0.04)/sum(E(genemania.network.graph.randwalk)$weight >= 0)
# [1] 0.2142857
# > sum(E(genemania.network.graph.randwalk)$weight >= 0.05)/sum(E(genemania.network.graph.randwalk)$weight >= 0)
# [1] 0.1785714
# > sum(E(genemania.network.graph.randwalk)$weight >= 0.06)/sum(E(genemania.network.graph.randwalk)$weight >= 0)
# [1] 0.1428571
# > sum(E(genemania.network.graph.randwalk)$weight >= 0.07)/sum(E(genemania.network.graph.randwalk)$weight >= 0)
# [1] 0.07142857
# > sum(E(genemania.network.graph.randwalk)$weight >= 0.08)/sum(E(genemania.network.graph.randwalk)$weight >= 0)
# [1] 0.07142857
# > sum(E(genemania.network.graph.randwalk)$weight >= 0.09)/sum(E(genemania.network.graph.randwalk)$weight >= 0)
# [1] 0.03571429
# > sum(E(genemania.network.graph.randwalk)$weight >= 0.10)/sum(E(genemania.network.graph.randwalk)$weight >= 0)
# [1] 0.03571429
plot(c(1:10/100), c(.71,.5,.32,.21,.17,.14,.07,.07,.03,.03), type = "l", 
     main="Percent of edges by weight threshold for randomly walked subgraph",
     xlab="Weight Threshold", ylab="Percent of Edges in Subgraph")

generateSubgraphSample <- function(g,snode,wt) {
  tg <- induced.subgraph(g,  random_walk(graph = g, start = c(snode), steps = c(4), mode = "all", stuck = "return")$name)
  #plot(tg, edge.color = ifelse(E(tg)$weight >= wt, "red", "black"))
  #hist(E(tg)$weight)
  return(  E(tg)$weight  )
}

generateSubgraphSample2 <- function(g,snode,wt) {
  tg <- induced.subgraph(g,  random_walk(graph = g, start = c(snode), steps = c(4), mode = "all", stuck = "return")$name)
  plot(tg, edge.color = ifelse(E(tg)$weight >= wt, "red", NA))
  return(  E(tg)$weight  )
}

generateSubgraphSample(genemania.network.graph, 1, 0.06)

rgweightsbrca1 <- c()
rgweightstp53 <- c()
for (i in 1:500) {
  rgweightsbrca1 <- append(rgweightsbrca1, generateSubgraphSample(genemania.network.graph, 1, 0))
  rgweightstp53 <- append(rgweightstp53, generateSubgraphSample(genemania.network.graph, 3, 0))
}

hist(rgweightsbrca1)
hist(rgweightstp53)
ecdf(rgweightsbrca1)
plot(ecdf(rgweightsbrca1))
abline(v = 0.10, h = 0.85, col=c("red"))
plot(ecdf(rgweightstp53))
abline(v = 0.10, h = 0.85, col=c("red"))

generateSubgraphSample2(genemania.network.graph, 1, 0.10)
generateSubgraphSample2(genemania.network.graph, 3, 0.10)

# Random Biological Graph Generator
# 1. Select N random genes from the gene list
sample(genenames, 10, replace=T)
# 2. Generate a network using GeneMANIA - This can be done because researchers are moving towards functional networks [aim 1].
# 3. Randomly sample a vertex from the network, and set a sample size - Edge weights are averaged, so only one edge will exist per dyad.


ensemblenames <- names(as.list(org.Hs.egENSEMBL2EG))
genemaniaAll <- read.csv('~/Code/data_for_phd_app/COMBINED.DEFAULT_NETWORKS.BP_COMBINING.csv')
genemaniaAll <- as.matrix(genemaniaAll)
gma <- graph.data.frame(d=genemaniaAll, directed = F)

E(gma)$Weight
E(gma)$weight <- E(gma)$Weight
gma <- remove.edge.attribute(gma, "Weight")

hist(as.numeric(E(gma)$weight), freq = F)

sample(V(gma)$name, 1)
which(V(gma)$name == "ENSG00000145476")
generateSubgraphSample2(gma, 8713, 0)

plot(induced.subgraph(gma, random_walk(graph = gma, start = which(V(gma)$name == sample(V(gma)$name, 1)), steps = c(4), mode = "all", stuck = "return")$name))

# Sample 34 networks
sampledRandomNetworkNodes <- sample(V(gma)$name, 34, replace = FALSE)
rgndat <- list()
for (s in 1:length(sampledRandomNetworkNodes)) {
  #print(sampledRandomNetworkNodes[s])
  cat(s, sampledRandomNetworkNodes[s],'\n')
  tt <- random_walk(graph = gma, start = which(V(gma)$name == sampledRandomNetworkNodes[s]), steps = c(4), mode = "all", stuck = "return")$name
  vgn <- unlist(as.list(org.Hs.egSYMBOL)[unlist(as.list(org.Hs.egENSEMBL2EG)[tt])])
  # When ensemble IDs are mapped to gene name, sometimes two different ensemble IDs have the same entrez gene ID
  isggma <- induced.subgraph(gma, tt)
  V(isggma)$label <- V(isggma)$name
  V(isggma)$name <- vgn
  V(isggma)$area <- pubMedCounts(isggma)
  V(isggma)$area[which(V(isggma)$area == 0)] <- format(0.0001, scientific=F)
  #V(isggma)$entrez <- V(isggma)$name
  V(isggma)$name <- V(isggma)$label
  isggma <- remove.vertex.attribute(isggma, "label")
  rgndat[[s]] = tpyFuncForRand(isggma)
  write(paste("var rn",s,' = ',rgndat[[s]],';\n', sep=''), file = '~/Code/VisualEncodingEngine/jsserver/datafiles.js', append = T)
  #print(rgndat[[s]])
  #plot(isggma, vertex.label=V(isggma)$name, vertex.size=sqrt(V(isggma)$area)*10, edge.width=exp(as.numeric(E(isggma)$weight)))
}


tpyFuncForRand <- function(g) {
  s = c();
  s = append(s, '[')
  iter = 0;
  vert.attrs <- list.vertex.attributes(g);
  edge.attrs <- list.edge.attributes(g);
  for (i in 1:length(V(g))) {
    iter = iter + 1; # changing i's to iter's
    s = append(s, paste('{ \"data\": { \"id\": "',iter,'", ', sep=''))
    for (k in 1:length(vert.attrs)) {
      if (vert.attrs[k] != "name") {
        s = append(s, paste('\"name\" : "', eval(parse(text=paste("V(g)$name",'[',iter,']',sep=''))),'", \"dimension\" : "',vert.attrs[k],'", \"value\" : "', eval(parse(text=paste("V(g)$",vert.attrs[k],'[',iter,']',sep=''))),'" ', sep='') )
      }
    }
    s = append(s, paste(' } },', sep=''))
  }
  
  for (e in 1:dim(unique(get.edgelist(g)))[1]) {
    #     cat( e,
    #          unique(get.edgelist(g))[e,1],
    #          unique(get.edgelist(g))[e,2],
    #          length(which(get.edgelist(g)[,1] == unique(get.edgelist(g))[e,1] & get.edgelist(g)[,2] == unique(get.edgelist(g))[e,2])),
    #          mean(E(g)$weight[which(get.edgelist(g)[,1] == unique(get.edgelist(g))[e,1] & get.edgelist(g)[,2] == unique(get.edgelist(g))[e,2])]),
    #          '\n')
    for (k in 1:length(edge.attrs)) {
      iter = iter + 1;
      if (  typeof(eval(parse(text=paste("E(g)$",edge.attrs[k],'[',e,']',sep='')))) == "character"  ) {
        cat('more to do','\n')
        edgeattrmean = as.numeric(eval(parse(text=paste("E(g)$",edge.attrs[k],'[',e,']',sep=''))))
        s = append(s, paste('{ \"data\": { \"id\": "',iter,'", ',
                            '\"source\": "',  which(V(g)$name == unique(get.edgelist(g))[e,1])  ,'", \"target\": "',  which(V(g)$name == unique(get.edgelist(g))[e,2])  ,'", ',
                            '\"dimension\": "', edge.attrs[k],'", \"value\":"',format(edgeattrmean, scientific=F),'"',
                            '} },', sep='') )
      }
      else {
        numerator = sum(as.numeric(E(g)$weight[which(get.edgelist(g)[,1] == unique(get.edgelist(g))[e,1] & get.edgelist(g)[,2] == unique(get.edgelist(g))[e,2])]))
        denominator = length(E(g)$weight[which(get.edgelist(g)[,1] == unique(get.edgelist(g))[e,1] & get.edgelist(g)[,2] == unique(get.edgelist(g))[e,2])])
        edgeattrmean = numerator / denominator
        #edgeattrmean = mean(E(g)$weight[which(get.edgelist(g)[,1] == unique(get.edgelist(g))[e,1] & get.edgelist(g)[,2] == unique(get.edgelist(g))[e,2])])
        s = append(s, paste('{ \"data\": { \"id\": "',iter,'", ',
                            '\"source\": "',  which(V(g)$name == unique(get.edgelist(g))[e,1])  ,'", \"target\": "',  which(V(g)$name == unique(get.edgelist(g))[e,2])  ,'", ',
                            '\"dimension\": "', edge.attrs[k],'", \"value\":"',format(edgeattrmean, scientific=F),'"',
                            '} },', sep='')
        )
      }
    }
  }
  s = append(s, ']')
  return( gsub("\"","'", gsub('},]', '}]', paste(s, collapse="")) ) )
}

write(toJSON(rgndat[[1]]), file = '~/Code/VisualEncodingEngine/jsserver/randomnetworks.json')


