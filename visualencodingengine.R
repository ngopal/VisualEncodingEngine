library(igraph)
library(combinat)

# iGraph Permutation
fake.df <- as.data.frame(cbind(c("A","A","A","B","C","D","E","L","N"),
                               c("B","E","D","C","D","F","D","N","A"),
                               c(4,2,1,6,3,2,3,1,6),
                               c("Relevant","Irrelevant","Relevant","Irrelevant","Irrelevant","Irrelevant","Relevant","Irrelevant","Relevant"),
                               c("One","Two","Three","Four","Five","Six","Seven","Eight","Nine")))
colnames(fake.df) <- c("to", "from", "weight", "group", "edgename")
fakeg <- graph.data.frame(fake.df, directed = F)
V(fakeg)$group <- c("Group2","Group1","Group3","Group1","Group2","Group2","Group1","Group3")
plot(fakeg)

colnames(fake.df)
colnames(prototypeMX)

# Dim1 --> Node Color --> V(graph)$color
# Dim3 --> Node Size  --> V(graph)$size

# DE Mapping generator function (takes in RF Model Prototype, graph.df)
# 

# DE Mapping input by user
# { "Dim1" : "Node Color",
#   "Dim2" : "Node Size" }

# Need function to output top 3 visual encodings based on task
ProvideTopNEncodingsByTask <- function(task,n) {
  #topThree <- colnames(prototypeMX)[which(prototypeMX[which(rownames(prototypeMX) == task),1:14] == 1)];
  topThree <- names(sort(rank(prototypeMX[which(rownames(prototypeMX) == task),1:14]), decreasing = T))[1:n];
  return(topThree);
}
topThreeVEbyask("Find Clusters")
topThreeVEbyask("Find Bridges")
topThreeVEbyask("Identify Nodes via Attributes")
topThreeVEbyask("Find Connected Components")
for (i in rownames(prototypeMX)) { cat(i, '  ||||  ', topThreeVEbyask(i),'\n') }



# Permutation function
# Takes in DE mapping, extracts keys, and returns permutations
DE_Mapping <- as.matrix(cbind(c("edgename", "group", "weight"), 
                              topThreeVEbyask("Find Connected Components")))
permuteDEMapping <- function(mapping) {
  perms <- permn(mapping[,1]);
  mappingList <- list();
  count = 1;
  for (i in perms) {
    #temptable <- as.matrix(cbind(i,mapping[,2:3]))
    temptable <- as.matrix(cbind(i,mapping[,2], mapping[match(i, mapping[,1]),3]))
    colnames(temptable) <- c("Data", "Encodings", "Type")
    mappingList[[ count ]] = temptable;
    count = count + 1;
  }
  return(mappingList)
}
DE_MappingList <- permuteDEMapping(DE_Mapping)
DE_MappingList[[4]]

# Lookup Table (connects RF model encoding names to iGraph)
# "Node Color" : "color"
# "Node Size"  : "size"
RF_iGraph_Mapping <- as.matrix(cbind(c("Node Hue", "Node Text", "Node Shape"), 
                                     c("color", "label", "shape")))
setGraphProperties <- function(graphObj,RFMap) {
  RFM <- RFMap;
  RFM[,2] <- paste(paste("V(", deparse(substitute(graphObj)),")$", sep=""), RF_iGraph_Mapping[,2], sep="");
  return(RFM);
}
graphPropertiesTable <- setGraphProperties(fakeg, RF_iGraph_Mapping)
eval(parse(text=graphPropertiesTable[1,2]))
plot(fakeg)

# Need a function to actually set the properties of graphs
V(fakeg)$color <- c("red","cyan","yellow","purple","blue","green", "red", "pink")


# Plot function (given by user: task, RF Prototype, graph)
visualizeGraph <- function(task, DEMap, graphObj) {
  return(1);
}

visualizeGraph("Find Connected Components", fakeg)

# When assigning encodings, read attributes of data dimensions and
# encode the data in a "cartographical" style. That is, hues are reduced
# to 9 colors at most, size and value are used for emaphasis, shape is ...

# Modify graph object 
# Report encoding risks: primarily if data type and encoding type do not match








plot(fakeg)
plot(fakeg, edge.width=E(fakeg)$weight, edge.label=E(fakeg)$edgename)
plot(fakeg, edge.width=E(fakeg)$weight, edge.label=E(fakeg)$weight)
plot(fakeg, edge.width=E(fakeg)$weight, edge.label=E(fakeg)$weight, edge.lty=ifelse(E(fakeg)$group == "Relevant", 1, 2))

# { edge.width : weight,
#   edge.label : edgename }

testMap <- as.matrix(cbind(c("edge.width", "edge.label", "vertex.color"), 
                           c("weight", "edgename", "group"),
                           c("edge","edge", "vertex")))

plotManifest <- function(graphObject, mappings) {
  
  poptions = c()
  for (i in 1:dim(mappings)[1]) {
    if (mappings[i,3] == "edge") {
      # E(fakeg)$group
      submittedData <- paste("E(",deparse(substitute(graphObject)),")$",as.character(mappings[i,2]), sep="")
      if( as.character(mappings[i,1]) == "edge.color" ) {
        # E(fakeg)$groupColor
        submittedDataWithGroup <- paste(submittedData,"Color",sep="")
        eval(parse(text=paste(submittedDataWithGroup,"<-","palette()[as.numeric(factor(",submittedData,"))]",sep="")))
        poptions <- c(poptions, paste(as.character(mappings[i,1]),"=", submittedDataWithGroup, sep="")) 
      }
      else{
        poptions <- c(poptions, paste(as.character(mappings[i,1]),"=", submittedData, sep="")) 
      }
    }
    else if (mappings[i,3] == "vertex") {
      # V(fakeg)$group
      submittedData <- paste("V(",deparse(substitute(graphObject)),")$",as.character(mappings[i,2]), sep="")
      if( as.character(mappings[i,1]) == "vertex.color" ) {
        # V(fakeg)$groupColor
        submittedDataWithGroup <- paste(submittedData,"Color",sep="")
        eval(parse(text=paste(submittedDataWithGroup,"<-","palette()[as.numeric(factor(",submittedData,"))]",sep="")))
        poptions <- c(poptions, paste(as.character(mappings[i,1]),"=", submittedDataWithGroup, sep="")) 
      }
      else{
        poptions <- c(poptions, paste(as.character(mappings[i,1]),"=", submittedData, sep="")) 
      }
    }
  }
  
  testCommand <- paste("plot(",deparse(substitute(graphObject)),",",paste(poptions, collapse=", "),")")
  
  eval(parse(text=testCommand))
}


# Must convert factor to number before inputting graph into function
# > V(fakeg)$group
# [1] "Group2" "Group1" "Group3" "Group1" "Group2" "Group2" "Group1" "Group3"
V(fakeg)$group <- palette()[as.numeric(factor(V(fakeg)$group))]

plotManifest(fakeg,testMap)

testMaps <- permuteDEMapping(testMap)
plotManifest(fakeg, testMaps[[1]])
plotManifest(fakeg, testMaps[[2]])
plotManifest(fakeg, testMaps[[3]]) #mappings, like this, have non-existant attributes mapped...
plotManifest(fakeg, testMaps[[4]])
plotManifest(fakeg, testMaps[[5]]) # here vertex.color is assigned to "edgename", which is an edge attribute...
plotManifest(fakeg, testMaps[[6]])

# These nonsensical mappings can be validated before submitting to the plot function. This can be done in the permuteDEMapping function, but is better completed
# in an independent step that "cleans" the DEMapping list.


rfmodeligraphMapping <- read.csv("~/Downloads/igraph mappings - Sheet1.csv")

#top3encodings --> rfModel --> igraph --> testMap

connectRFModel_to_iGraph <- function(rfItems) {
  return( rfmodeligraphMapping[match(rfItems, rfmodeligraphMapping[,2]),3] )
}

connectRFModel_to_iGraph(topThreeVEbyask("Find Connected Components"))

testMap_2 <- as.matrix(cbind(c("vertex.label", "vertex.color", "edge.color"), 
                           c("names", "group", "weight"),
                           c("vertex", "vertex", "edge")))
V(fakeg)$names <- c("Gene1","Gene2","Gene3","Gene4","Gene5","Gene6","Gene7","Gene8")
testMaps_2 <- permuteDEMapping(testMap_2)
plotManifest(fakeg, testMaps_2[[1]])
plotManifest(fakeg, testMaps_2[[2]])
plotManifest(fakeg, testMaps_2[[3]])
plotManifest(fakeg, testMaps_2[[4]])
plotManifest(fakeg, testMaps_2[[5]])
plotManifest(fakeg, testMaps_2[[6]])


#connectRFModel_to_iGraph(topThreeVEbyask("Find Clusters"))
connectRFModel_to_iGraph(topThreeVEbyask("Identify Nodes via Attributes"))
testMap_3 <- as.matrix(cbind(c("vertex.size", "vertex.shape", "edge.color"), 
                             c("names", "group", "group"),
                             c("vertex", "vertex", "edge")))
testMaps_3 <- permuteDEMapping(testMap_3)
plotManifest(fakeg, testMaps_3[[1]])
plotManifest(fakeg, testMaps_3[[2]])
plotManifest(fakeg, testMaps_3[[3]])
plotManifest(fakeg, testMaps_3[[4]])
plotManifest(fakeg, testMaps_3[[5]])
plotManifest(fakeg, testMaps_3[[6]])

# I need to include logic that converts categorical data into numeric representations
categoricalToNumeric <- function(categoricalData) {
  return( as.numeric(as.factor(categoricalData)) )
}

numericToTwoClasses <- function(numericalData, cutoff) {
  classedData <- ifelse(as.numeric(numericalData) > cutoff, 1, 2);
  return( classedData )
}

numericToTwoClasses(E(fakeg)$weight, 3)


# categorical to categorical conversions --> "Relevant" = "red"
V(fakeg)$group <- palette()[as.numeric(factor(V(fakeg)$group))]

# numeric to categorical conversions -->  ">= 40" is "group1", <40 is "group2"


# Objective 1 - Given a task, what encodings should I use?
ProvideTopNEncodingsByTask("Find Clusters", 10)
for (i in rownames(prototypeMX)) { cat(i, '  ||||  ', ProvideTopNEncodingsByTask(i,5),'\n') }

# Objective 2 - Given a set of tasks, which encodings should optimally be used for which task?

# T1 : ABCDEF
# T2 : ABCGHD
# T3 : ABCFGQ
# 1. Push each encoding into hash and record task name
# 2. Look through hash for items with task length 1, push item into new array for that task
# 3. Return list of task names and encodings that are unique to each
# -> { T1:E, T2:H, T3:Q }

uniqueEncodingsFromItems <- function(...) {
  encodingList <- list();
  args <- list(...)
  for (i in 1:length(args)) {
    for (enc in args[[i]]) {
      #cat(i,enc,'\n')
      if( !(enc %in% names(encodingList)) ) {
        encodingList[[enc]] <- c(i)
      }
      else {
        encodingList[[enc]] <- c(encodingList[[enc]], i)
      }
    }
  }
  return(encodingList)
}

#uniqueEncodingsFromItems(c(1,2,3),c(6,7,8),c(9,10,11))
toyList <- uniqueEncodingsFromItems(ProvideTopNEncodingsByTask("Find Connected Components", 5),ProvideTopNEncodingsByTask("Find Clusters", 5))
for (i in 1:length(toyList)) { if(length(toyList[[i]]) == 1) { cat(names(toyList[i]), toyList[[i]],'\n') } }

toyList <- uniqueEncodingsFromItems(ProvideTopNEncodingsByTask("Find Connected Components", 10),ProvideTopNEncodingsByTask("Find Clusters", 10))
for (i in 1:length(toyList)) { if(length(toyList[[i]]) == 1) { cat(names(toyList[i]), toyList[[i]],'\n') } }

toyList <- uniqueEncodingsFromItems(ProvideTopNEncodingsByTask("Find Connected Components", 10),ProvideTopNEncodingsByTask("Find Clusters", 10),ProvideTopNEncodingsByTask("Find Bridges", 10))
for (i in 1:length(toyList)) { if(length(toyList[[i]]) == 1) { cat(names(toyList[i]), toyList[[i]],'\n') } }

# Objective 3 - Given a set of tasks, which encodings should be used for efficiency?

library(reshape)
meltedprototypeMX <- melt(apply(prototypeMX[,1:14], MARGIN=c(1), FUN=rank)) #should use ties.method = "first" if possible
pmxg <- graph.data.frame(meltedprototypeMX, directed=F)
V(pmxg)$type <- c(rep(TRUE,10),rep(FALSE,14))
plot(pmxg, edge.width=E(pmxg)$value, vertex.color = c(rep("tomato",14),rep("gray80",10)))
pmxgMF <- max_flow(pmxg, source=V(pmxg)["Find Common Connection"], target=V(pmxg)["Identify Nodes via Attributes"])

plot(pmxg, edge.width=E(pmxg)$value/5, vertex.color=ifelse(V(pmxg)$type, "tomato", "gray80"), layout=layout.bipartite)
#plot(pmxg, edge.width=E(pmxg)$value/5, vertex.color=ifelse(V(pmxg)$type, "tomato", "gray80"), layout=layout.bipartite, edge.color=ifelse(pmxgMF$flow == -1, "black", "white"))
plot(pmxg, edge.width=E(pmxg)$value/5, vertex.color=ifelse(V(pmxg)$type, "tomato", "gray80"), layout=layout.fruchterman.reingold, edge.color=ifelse(pmxgMF$flow == -1, "black", "white"))

pmxg.subg <- induced_subgraph(pmxg, c(1:14,16,22))
V(pmxg.subg)$type <- c(rep(TRUE,14),rep(FALSE,2))
pmxgMF <- max_flow(pmxg.subg, source=V(pmxg.subg)["Find Common Connection"], target=V(pmxg.subg)["Identify Nodes via Attributes"])
plot(pmxg.subg, edge.width=E(pmxg.subg)$value/5, vertex.color=ifelse(V(pmxg.subg)$type, "tomato", "gray80"), layout=layout.fruchterman.reingold, edge.color=ifelse(pmxgMF$flow == -1, "black", "white"))


# Comparing 2 tasks, finding optimal assignment
ttemp <- apply(prototypeMX[,1:14], MARGIN=c(1), FUN=rank)[,c(4,8)]
ttemp[ttemp[,1] > ttemp[,2],]
ttemp[ttemp[,2] > ttemp[,1],]

names(sort(ttemp[ttemp[,1] > ttemp[,2],1], decreasing = TRUE)) # optimal assignment for 1
names(sort(ttemp[ttemp[,2] > ttemp[,1],2], decreasing = TRUE)) # optimal assignment for 2

# Comparing 3 tasks
# Comparing 2 tasks, finding optimal assignment
ttemp3 <- apply(prototypeMX[,1:14], MARGIN=c(1), FUN=rank)[,c(4,5,8)]
ttemp3_o <- apply(ttemp3, FUN=order, MARGIN=1) # take max
rownames(ttemp3_o) <- rownames(prototypeMX)[c(4,5,8)]

names(ttemp3_o[1,ttemp3_o[1,] == 3])
names(ttemp3_o[2,ttemp3_o[2,] == 3])
names(ttemp3_o[3,ttemp3_o[3,] == 3])


# Finding most efficient encodings given task
ttemp_sum <- apply(ttemp, FUN=sum, MAR=1)
names(sort(ttemp_sum, decreasing = T)) # most efficient ordering of encodings given task

ttemp3_sum <- apply(ttemp3, FUN=sum, MAR=1)
names(sort(ttemp3_sum, decreasing = T))




----------playig with ideads-------



g <- graph.formula( a-b-c-d-e-f )
m1 <- c("b", "a", "d", "c", "f", "e")   # maximal matching
m4 <- c("1", "2", "3", "4", "5", "6")   # maximal matching
m2 <- c("b", "a", "d", "c", NA, NA)     # non-maximal matching
m3 <- c("b", "c", "d", "c", NA, NA)     # not a matching
is.matching(g, m1)
is.matching(g, m2)
is.matching(g, m3)
is.maximal.matching(g, m1)
is.maximal.matching(g, m2)
is.maximal.matching(g, m3)

V(g)$type <- c(FALSE,TRUE)
str(g, v=TRUE)
maximum.bipartite.matching(g)

g2 <- graph.formula( a-b-c-d-e-f-g )
V(g2)$type <- rep(c(FALSE,TRUE), length=vcount(g2))
str(g2, v=TRUE)
maximum.bipartite.matching(g2)



#create the graph
N <- 5
g3 <- graph.full.bipartite (N,N)
#Name the vertices A1...AN and B1..BN
V(g3)$name <- c(paste0("A", 1:N), paste0("B", 1:N))
#set the edge weights
set.seed(122)
E(g3)$weight <- sample(10,N^2, replace=T) #use your fWgt function here instead

#verifty if we did things right
str(g3, TRUE)
is.bipartite(g3)

maximum.bipartite.matching(g3)
#$matching_size
#[1] 5
#
#$matching_weight
#[1] 37
# 
#$matching
#  A1   A2   A3   A4   A5   B1   B2   B3   B4   B5 
#"B1" "B2" "B4" "B3" "B5" "A1" "A2" "A4" "A3" "A5" 

prototypeMX[,1:14]
g <- graph.data.frame(melt(prototypeMX[1:10,1:14]), directed = F)
V(g)$type <- c(rep(TRUE, 10),rep(FALSE,14))
is.bipartite(g)
maximum.bipartite.matching(g)

sg <- induced.subgraph(g, c(4,5,11:24))
plot(sg)
maximum.bipartite.matching(sg)


IterativeBipartiteMatching <- function(inputTable) {
  g <- graph.data.frame(melt(inputTable), directed = F)
  V(g)$type <- c(rep(TRUE, dim(inputTable)[1]),rep(FALSE,dim(inputTable)[2]))
  
  taskList <- list()
  
  for (t in 1:length(colnames(inputTable))) {
    taskList[[ colnames(inputTable)[t] ]] <- vector()
  }
  
  if (is.bipartite(g)) {
    while (  length(V(g)) > length(colnames(inputTable))  ) {
      maximum.bipartite.matching(g)
      matchedData <- maximum.bipartite.matching(g)$matching[!is.na(maximum.bipartite.matching(g)$matching)]
      
      for (t in 1:length(colnames(inputTable))) {
        if (  colnames(inputTable)[t] %in% matchedData  ) {
          taskList[[ colnames(inputTable)[t] ]] <- c(taskList[[ colnames(inputTable)[t] ]], names(which(matchedData == colnames(inputTable)[t])))
          g <- delete_vertices(g, names(which(matchedData == colnames(inputTable)[t])))
        }
      }
    }
    
  }
  else {
    cat("NOT BIPARTITE!\n")
  }
  
  resuT <- c()
  for (l in 1:length(names(taskList))) {
    for (enc in 1:length(taskList[[l]])) {
      resuT <- rbind(resuT,c(taskList[[l]][enc],names(taskList)[l]))
    }
  }
  
  return( resuT[order(resuT[,1]),] )
  
}

IterativeBipartiteMatching(inputTable)


# finding all combinations by brute force for 4 cols takes 331776 iterations,
# so if I can't use a solver, then a non-deterministic optimization approach
# may be more suitable...

intersectingEncodingsByTask <- function(tasks) {
  temppmx <- prototypeMX[tasks,1:14]
  temppmx <- apply(temppmx, FUN=rank, MARGIN=1)
  
  assignmentMat <- matrix(0, dim(temppmx)[1], dim(temppmx)[2])
  colnames(assignmentMat) <- colnames(temppmx)
  rownames(assignmentMat) <- rownames(temppmx)
  
  resVec <- c();

  for (i in 1:dim(temppmx)[1]) {
    ttt <- which(rank(apply(diag(dim(temppmx)[2]) * temppmx[i,], FUN=sum, MARGIN=1), ties.method = "random") == dim(temppmx)[2])
    cat(i, temppmx[i,],ttt,'\n')
    resVec <- rbind(resVec, ttt)
  }

  return(cbind(colnames(temppmx)[as.numeric(resVec)], rownames(temppmx)))
}

intersectingEncodingsByTask(c(4,5,7))
intersectingEncodingsByTask(c(1,8,3))
intersectingEncodingsByTask(c(3,2,5))
intersectingEncodingsByTask(c(3,2,7,9))
intersectingEncodingsByTask(c(4,8,3))
intersectingEncodingsByTask(c(2,3,4))





GeneticAlgorithmApproach <- function(inputTable) {
  
  library(genalg)
  toympx <- melt(inputTable)
  
  # penalize if same encoding assigned for more than one task
  
  evalFunc <- function(chr) {
    
    # Set penalty constants
    penalty = 0;
    sameEncodingPenalty = 100;
    moreThanOnePenalty = 100;
    zeroPenalty = 100;
    
    # add penalty if same encoding used
    tma <- t(matrix(chr, dim(inputTable)[1], dim(inputTable)[2]))
    if (  sum(apply(tma, FUN=sum, MARGIN=2) > 1) > 0  ) {
      penalty = penalty + sameEncodingPenalty
    }
    
    # add penalty if all values are zero
    if ( sum(chr) == 0 ) {
      penalty = penalty + zeroPenalty;
    }
    
    return(  (sum(chr * toympx[,3]) * -1) + penalty  )
  }
  
  iter = 150
  GAmodel <- rbga.bin(size = dim(inputTable)[1]*dim(inputTable)[2], popSize = 250, iters = iter, mutationChance = 0.10, 
                      elitism = T, evalFunc = evalFunc)
  mostFitPop <- GAmodel$population[which(GAmodel$evaluations == min(GAmodel$evaluations)),]
  
  if (length(mostFitPop) > dim(inputTable)[1]*dim(inputTable)[2]) {
    GAresults <- toympx[as.logical(mostFitPop[sample(dim(mostFitPop)[1], 1),]),]
    return( GAresults[order(GAresults[,1]),] )
  }
  else {
    GAresults <- toympx[as.logical(mostFitPop),]
    return( GAresults[order(GAresults[,1]),] )
  }
}


GeneticAlgorithmApproach(apply(prototypeMX[,1:14], FUN=rank, MARGIN=1)[,c(3,4,5)])
GeneticAlgorithmApproach(apply(prototypeMX[,1:14], FUN=rank, MARGIN=1)[,c(3,4,8)])




# Above code from here: http://www.harlan.harris.name/2011/05/optimizing-meat-shares-details/
# needed to brew install symphony, which needs to tap "brew tap coin-or-tools/coinor"

LinearProgrammingApproach <- function(inputTable, balanced = FALSE) {
  
  install.packages("Rsymphony", repos="http://R-Forge.R-project.org")
  library(Rsymphony)
  library(plyr)
  
  num.items <- dim(inputTable)[1] #encodings
  num.pers <- dim(inputTable)[2]  #tasks
  item.names <- rownames(inputTable)
  pers.names <- colnames(inputTable)
  obj <- as.matrix(inputTable)
  
  cat('num.items', num.items, '\n')
  cat('num.pers', num.pers, '\n')
  cat('item.names', item.names, '\n')
  cat('pers.names', pers.names, '\n')
  cat('obj', obj, '\n')
  
  mat.0 <- obj*0
  mat.utility.0 <- obj.utility*0 # want this later
  
  mat <- laply(1:num.items, function(ii) { x <- mat.0; x[ii, ] <- 1; as.double(x) })
  dir <- rep('<=', num.items)
  rhs <- rep(1, num.items)
  
  types <- rep('B', num.items * num.pers)
  max <- TRUE # maximizing utility
  soln <- Rsymphony_solve_LP(obj, mat, dir, rhs, types=types, max=max)
  
  # compute the utility of that assignment for each person
  soln2utility <- function(obj, soln, groups) {
    llply(split(soln[seq_along(groups)] * obj[seq_along(groups)], groups), sum)
  }
  
  # take the solution vector, split it, and collapse as comma-sep strings
  soln2assign <- function(soln, groups) {
    llply(split(soln[seq_along(groups)], groups), 
          function(x) paste(which(as.logical(x)), collapse=', '))
  }
    
  if (!balanced) {

    # which items go with which person?
    pers.groups <- as.vector(matrix(1:num.pers, nrow=num.items, ncol=num.pers, byrow=TRUE))
    items.list <- soln2assign(soln$solution, pers.groups)
    pers.utility <- soln2utility(obj, soln$solution, pers.groups)
    
    lpResult <- c()
    
    for (i in 1:num.pers) {
      lpResult <- rbind(lpResult, t(rbind(names(inputTable[as.integer(unlist(strsplit(items.list[[i]],', '))),i]),rep(colnames(inputTable)[i],length(names(inputTable[as.integer(unlist(strsplit(items.list[[i]],', '))),i]))))))
    }
    
    cat(sprintf('BALANCED=FALSE Task #%d got Encodings %s worth %0.2f\n',
                1:num.pers,
                items.list,
                pers.utility), sep='')
    
    return(lpResult[order(lpResult[,1]),])
    
  }
  else {
    lambda <- 1
    obj.parity <- c(-lambda, -(-lambda))
    # later on, we'll turn obj.utility into a vector, and tag these on to it
    
    # add two columns of 0s to the existing constraints, for parity
    mat <- cbind(mat, 0, 0)
    
    d.constraint <- function(iperson, ul) { # ul = 1 for upper, 0 for lower
      x<- mat.utility.0
      x[, iperson ] <- 1 
      x <- x * obj.utility
      c(as.double(x), (if (ul) c(-1,0) else c(0,-1)))
    }
    mat <- rbind(mat, maply(expand.grid(iperson=1:num.pers, ul=c(1,0)), d.constraint, .expand=FALSE))
    dir <- c(dir, c(rep('<=', num.pers), rep('>=', num.pers)))
    rhs <- c(rhs, rep(0, num.pers*2))
    
    num.bool.consts <- num.items * num.pers
    num.cont.consts <- 2
    
    types <- c(rep('B', num.bool.consts), rep('C', num.cont.consts))
    max <- TRUE # maximizing utility
    
    obj <- c(as.numeric(obj.utility), obj.parity)
    soln <- Rsymphony_solve_LP(obj, mat, dir, rhs, types=types, max=max)
    
    items.list <- soln2assign(soln$solution, pers.groups)
    pers.utility <- soln2utility(obj, soln$solution, pers.groups)
    
    lpResult <- c()
    
    for (i in 1:num.pers) {
      lpResult <- rbind(lpResult, t(rbind(names(inputTable[as.integer(unlist(strsplit(items.list[[i]],', '))),i]),rep(colnames(inputTable)[i],length(names(inputTable[as.integer(unlist(strsplit(items.list[[i]],', '))),i]))))))
    }
    
    cat(sprintf('BALANCED=TRUE Task #%d got Encodings %s worth %0.2f\n',
                1:num.pers,
                items.list,
                pers.utility), sep='')
    
    return(lpResult[order(lpResult[,1]),])
    
    
  }
  
}




LinearProgrammingApproach(apply(prototypeMX[,1:14], FUN=rank, MARGIN=1)[,c(3,4,5)], balanced=FALSE)
LinearProgrammingApproach(apply(prototypeMX[,1:14], FUN=rank, MARGIN=1)[,c(3,4,5)], balanced=TRUE)


generateRandomRankedTable <- function(enc = 14, tasks = 3) {
  return( apply(matrix(rlnorm(enc*tasks), nrow=enc), FUN=rank, MARGIN=2) )
}


compareAllMethods <- function(inputTable) {
  tt1 <- Sys.time()
  tab1 <- IterativeBipartiteMatching(inputTable)
  tt2 <- Sys.time()
  tt3 <- Sys.time()
  tab2 <- GeneticAlgorithmApproach(inputTable)
  tt4 <- Sys.time()
  tt5 <- Sys.time()
  tab3 <- LinearProgrammingApproach(inputTable, balanced=FALSE)
  tt6 <- Sys.time()
  tt7 <- Sys.time()
  tab4 <- LinearProgrammingApproach(inputTable, balanced=TRUE)
  tt8 <- Sys.time()
  
  time1 <- as.numeric(tt2-tt1)
  time2 <- as.numeric(tt4-tt3)
  time3 <- as.numeric(tt6-tt5)
  time4 <- as.numeric(tt8-tt7)
  cat("Time",time1,time2,time3,time4,"",'\n')
  allTab <- as.data.frame(cbind(tab1,colnames(inputTable1)[tab2[,2]],tab3[,2],tab4[,2]))
  #allTab <- rbind(allTab, c("Time",time1,time2,time3,time4,""))
  colnames(allTab) <- c("Encoding","IBM", "GA", "LP", "LPBalanced")
  
  return(  allTab  )
  
}


#### Let's Compare All of  Them ####
# optional intersectingEncodingsByTask(c(4,5,7)), since it takes 0-1 matrix
# and not ranking...

# Generate random selections of my own data tables
inputTable1 <- apply(prototypeMX[,1:14], FUN=rank, MARGIN=1)[,c(3,4,5)]
inputTable2 <- apply(prototypeMX[,1:14], FUN=rank, MARGIN=1)[,c(3,8,1)]
inputTables <- list()
for (l in 1:100) {
  inputTables[[l]] <- apply(prototypeMX[,1:14], FUN=rank, MARGIN=1)[,sample(1:10,sample(1:10,1))]
}

# Generate random ranked tables
randomTables <- list()
for (l in 1:100) {
  randomTables[[l]] <- generateRandomRankedTable(enc = sample(1:100,1), tasks = sample(1:100,1))
}



IterativeBipartiteMatching(inputTable1)
GeneticAlgorithmApproach(inputTable1)
LinearProgrammingApproach(inputTable1, balanced=FALSE)
LinearProgrammingApproach(inputTable1, balanced=TRUE)

IterativeBipartiteMatching(inputTable2)
GeneticAlgorithmApproach(inputTable2)
LinearProgrammingApproach(inputTable3, balanced=FALSE)


compareInput1 <- compareAllMethods(inputTable1)
compareInput2 <- compareAllMethods(inputTable2)

#compareAllMethods(inputTables[[40]])




