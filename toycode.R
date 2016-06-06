library(igraph)

g <- watts.strogatz.game(1, 100, 5, 0.05)
#g <- erdos.renyi.game(100, 3/100)
plot(g, vertex.size=2, vertex.label=NA)

# Calculate edge betweenness
eb <- cbind(E(g),edge_betweenness(g))
ebt <- eb[order(eb[,2], decreasing = T),]
head(ebt)
plot(ebt[,2], main="distribution of edge betweenness", ylab="Edge Between Score")
abline(h=35, col="red")
abline(v=100, col="red")

el <- get.edgelist(g)[as.vector(tail(ebt, 500-50)[,1]),] #edges as edgelist
elg <- graph_from_edgelist(el, directed=FALSE)
plot(elg, vertex.size=2, vertex.label=NA)

# Use low edge between scores to make minimum span tree
mst <- minimum.spanning.tree(elg)
layout.fruchterman.reingold(mst)
plot(mst, vertex.size=2, vertex.label=NA)

# FD layout
layout.fruchterman.reingold(mst)

# reinsert high edge between graph one at a time, starting with the highest

for (i in 1:dim(ebt)[1]) {
  new_el <- get.edgelist(g)[as.vector(ebt[i,1]),]
  el <- rbind(new_el,el) # note this will recalculate layout everytime, rather than just adding edges
  elg <- graph_from_edgelist(el, directed=FALSE)
  plot(elg, vertex.size=2, vertex.label=NA)
  readline(prompt="Press [ENTER] to continue")
}


# Everything below is unfinished an experimental
entropy.binary <- function(line) {
  p <- mean(line)
  return(-p*log2(p) - (1 - p)*log2((1 - p)))
}

slider <- function(line) {
  former = 1
  vec <- c()
  for (i in 1:(length(line)-1)) {
    pair <- line[c(former,former+1)]
    former <- former + 1
    vec <- rbind(vec,pair)
  }
  return(vec)
}

binary_entropy <- function(graphObject) {
  # 1. Calculate all paths
  # 2. See how many edges make up each path. If an edge is part of path, then 1, else 0
  edgeMatrix <- matrix(0,length(E(g)),length(E(g)))
  paths_list <- get.all.shortest.paths(g, from=V(g), to=V(g))
  
  
  for (i in 2:length(paths_list$res)) {
    at_hand <- slider(paths_list$res[[i]])
    for (k in 1:dim(at_hand)[1]) {
      edgeMatrix[at_hand[k,1],at_hand[k,2]] <- 1
    }
  }
  
  return(edgeMatrix)
  
}

edgeMatrixforGraph <- binary_entropy(g)
tm <- cbind(1:length(E(g)),apply(edgeMatrixforGraph, MARGIN=c(1), FUN=entropy.binary))
tm <- tm[order(tm[,2]),]
tm <- tm[!is.nan(tm[,2]),] #removing NaNs. Now col 1 is edge number and 2 is score

----------------------
  
V(g)$degree <- degree(g)
V(g)$betweenness <- betweenness(g)
V(g)$closeness <- closeness(g)
V(g)$eigen <- eigen_centrality(g)$vector
V(g)$page <- page.rank(g)$vector

testScale <- colorRampPalette((brewer.pal(9, "Reds")))


assignColor <- function(maxVal, minVal, valArray) {
  indexes <- valArray - minVal + 1
  return(testScale(length(valArray))[floor(rank(valArray))])
}

par(mfrow=c(2,2),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)
set.seed(1218430)
pg1 <- plot(g, vertex.size=10, vertex.label=NA, 
     vertex.color=assignColor(max(V(g)$degree), min(V(g)$degree), V(g)$degree),
     main="Degree Centrality")
set.seed(1218430)
pg2 <- plot(g, vertex.size=10, vertex.label=NA, 
     vertex.color=assignColor(max(V(g)$betweenness), min(V(g)$betweenness), V(g)$betweenness),
     main="Betweenness Centrality")
set.seed(1218430)
pg3 <- plot(g, vertex.size=10, vertex.label=NA, 
     vertex.color=assignColor(max(V(g)$closeness), min(V(g)$closeness), V(g)$closeness),
     main="Closeness Centrality")
set.seed(1218430)
pg4 <- plot(g, vertex.size=10, vertex.label=NA, 
     vertex.color=assignColor(max(V(g)$eigen), min(V(g)$eigen), V(g)$eigen),
     main="Eigenvactor Centrality")
dev.off()

  
centrality_table <- as.table(cbind(V(g)$degree,
  V(g)$betweenness,
  V(g)$closeness,
  V(g)$eigen))
colnames(centrality_table) <- c("degree","between","closeness","eigen")
parcoord(centrality_table)
centrality_table_pct <- matrix(NA,100,4)
centrality_table_pct[,1] <- centrality_table[,1]/max(centrality_table[,1])
centrality_table_pct[,2] <- centrality_table[,2]/max(centrality_table[,2])
centrality_table_pct[,3] <- centrality_table[,3]/max(centrality_table[,3])
centrality_table_pct[,4] <- centrality_table[,4]/max(centrality_table[,4])
avg_centrality <- apply(centrality_table_pct, FUN=mean, MARGIN=c(1))

par(mfrow=c(1,2),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)
parcoord(centrality_table)
set.seed(1218430)
pg1 <- plot(g, vertex.size=10, vertex.label=NA, 
            vertex.color=assignColor(max(V(g)$degree), min(V(g)$degree), V(g)$degree),
            main="Degree Centrality")
dev.off()

par(mfrow=c(1,2),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)
parcoord(centrality_table_pct, col=assignColor(max(avg_centrality), min(avg_centrality), avg_centrality))
set.seed(1218430)
pg1 <- plot(g, vertex.size=10, vertex.label=NA, 
            vertex.color=assignColor(max(avg_centrality), min(avg_centrality), avg_centrality),
            main="Avg Centrality")
dev.off()

library(ks)
data(faithful)
H <- Hpi(x=faithful)
fhat <- kde(x=faithful, H=H)
plot(fhat, display="filled.contour2")
points(faithful, cex=0.5, pch=16)

N <- Hpi(lmds)
N <- Hpi(lfr)
nhat <- kde(x=lmds, H=N)
plot(nhat, display="filled.contour2")
points(lmds, cex=0.5, pch=16)
plot(toyg, layout=lmds)
plot(toyg, layout=lfr)
par(new=TRUE)



# Simple Hilbert
Hilbert <- function(level=6, x=0, y=0, xi=1, xj=0, yi=0, yj=1) {
  if (level <= 0) {
    return(c(x + (xi + yi)/2, y + (xj + yj)/2))
  } else {
    return(rbind(
      Hilbert(level-1, x,           y,           yi/2, yj/2,  xi/2,  xj/2),
      Hilbert(level-1, x+xi/2,      y+xj/2 ,     xi/2, xj/2,  yi/2,  yj/2),
      Hilbert(level-1, x+xi/2+yi/2, y+xj/2+yj/2, xi/2, xj/2,  yi/2,  yj/2),
      Hilbert(level-1, x+xi/2+yi,   y+xj/2+yj,  -yi/2,-yj/2, -xi/2, -xj/2)
    ))
  }
}


plot(Hilbert(),type="b", col=blue2red(dim(Hilbert())[1]))
hilb <- cbind(Hilbert(level=4),round(vcount(testgraph2)/256*1:256),rep("white",256))
hilb[which(hilb[,3] == 17), 4] <- "black"
plot(Hilbert(),type="b", col=blue2red(max(hilb[,3])))
plot(Hilbert(level=4),type="b", col=hilb[,4])

hilb <- cbind(Hilbert(level=3),round(vcount(testgraph5)/64*1:64),rep("white",64))
hilb[which(hilb[,3] == 17), 4] <- "black"
plot(Hilbert(level=3),type="b", col=hilb[,4])


plot(Hilbert(level=3),type="b", col=blue2red(dim(Hilbert(level=3))[1]))
plot(Hilbert(level=4),type="b", col=blue2red(dim(Hilbert(level=4))[1]))
plot(Hilbert(level=5),type="b", col=blue2red(dim(Hilbert(level=5))[1]))
plot(Hilbert(level=6),type="b", col=blue2red(dim(Hilbert(level=6))[1]))
plot(Hilbert(level=7),type="b", col=blue2red(dim(Hilbert(level=7))[1]))
plot(Hilbert(level=8),type="b", col=blue2red(dim(Hilbert(level=8))[1]))
plot(Hilbert(level=9),type="b", col=blue2red(dim(Hilbert(level=9))[1]))

points(Hilbert(level=8)[dim(Hilbert(level=8))[1]/2,], col="black")
points(Hilbert(level=7)[dim(Hilbert(level=7))[1]/2,], col="black")
points(Hilbert(level=6)[dim(Hilbert(level=6))[1]/2,], col="black")
points(Hilbert(level=3)[dim(Hilbert(level=3))[1]/2,], col="black")
points(Hilbert(level=4)[dim(Hilbert(level=4))[1]/2,], col="black")

# Hilbert Space Example
devtools::install_github("vz-risk/ipv4heatmap")
library(ipv4heatmap)
library(data.table)

# read in cached copy of blocklist.de IPs - orig URL http://www.blocklist.de/en/export.html
hm <- ipv4heatmap(readLines("http://dds.ec/data/all.txt"))

# read in CIDRs for China and North Korea
cn <- read.table("http://www.iwik.org/ipcountry/CN.cidr", skip=1)
kp <- read.table("http://www.iwik.org/ipcountry/KP.cidr", skip=1)

# make bounding boxes for the CIDRs

cn_boxes <- rbindlist(lapply(boundingBoxFromCIDR(cn$V1), data.frame))
kp_box <- data.frame(boundingBoxFromCIDR(kp$V1))

# overlay the bounding boxes for China onto the IPv4 addresses we read in and Hilbertized

gg <- hm$gg
gg <- gg + geom_rect(data=cn_boxes, 
                     aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), 
                     fill="white", alpha=0.2)
gg <- gg + geom_rect(data=kp_box, 
                     aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), 
                     fill="white", alpha=0.2)

gg



# Pathway Analysis
# These are bioconductor packages. See http://www.bioconductor.org/install/ for installation instructions
library(Biobase)
library(GEOquery)
library(limma)
library(SPIA)
library(hgu133plus2.db)

# load series and platform data from GEO: 
# http://www.ncbi.nlm.nih.gov/projects/geo/query/acc.cgi?acc=GSE4107
eset <- getGEO("GSE4107", GSEMatrix =TRUE)[[1]] 

# log transform 
exprs(eset) <- log2(exprs(eset))

# set up a design matrix and contrast matrix
design <- model.matrix(~0+as.factor(c(rep(1,12), rep(0,10))))
colnames(design) <- c("cancer","normal")
contrast.matrix <- makeContrasts(cancer_v_normal=cancer-normal, levels=design)

# run the analysis with empirical Bayes moderated standard errors
fit <- lmFit(eset,design)
fit2 <- contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit2)


# get useful information for the top 25 genes
top <- topTable(fit2, coef="cancer_v_normal", number=nrow(fit2), adjust.method="fdr")
top <- na.omit(subset(top, select=c(ID, logFC, adj.P.Val)))
top$ID <- as.character(top$ID)

# annotate with entrez info
top$ENTREZ<-unlist(as.list(hgu133plus2ENTREZID[top$ID]))
top<-top[!is.na(top$ENTREZ),]
top<-top[!duplicated(top$ENTREZ),]
top$SYMBOL<-unlist(as.list(hgu133plus2SYMBOL[top$ID]))
top<-top[!is.na(top$SYMBOL),]
top<-top[!duplicated(top$SYMBOL),]

top[1:20,]

# significant genes is a vector of fold changes where the names
# are ENTREZ gene IDs. The background set is a vector of all the 
# genes represented on the platform.
sig_genes <- subset(top, adj.P.Val<0.01)$logFC
names(sig_genes) <- subset(top, adj.P.Val<0.01)$ENTREZ
all_genes <- top$ENTREZ

# run SPIA.
spia_result <- spia(de=sig_genes, all=all_genes, organism="hsa", plots=TRUE)

# Once you start running SPIA you'll see it go through all the KEGG pathways
# for your organism. This will take a few minutes! Be patient.
# Done pathway 1 : RNA transport..
# Done pathway 2 : RNA degradation..
# Done pathway 3 : PPAR signaling pathway..
# Done pathway 4 : Fanconi anemia pathway..
# Done pathway 5 : MAPK signaling pathway..
# Done pathway 6 : ErbB signaling pathway..
# Done pathway 7 : Calcium signaling pathway..
# Done pathway 8 : Cytokine-cytokine receptor int..
# Done pathway 9 : Chemokine signaling pathway..
# Done pathway 10 : Neuroactive ligand-receptor in..

head(spia_result)
plotP(spia_result, threshold=0.05)







# Network Dashboard
library(igraph)
library(MASS)

hist(centralization.betweenness(testgraph3)$res)
hist(centralization.closeness(testgraph3)$res)
hist(centralization.degree(testgraph3)$res)
hist(centralization.evcent(testgraph3)$vector)

plot(sort(degree(testgraph3)))
plot(sort(betweenness(testgraph3)))
plot(sort(closeness(testgraph3)))
plot(sort(eigen_centrality(testgraph3)$vector))

paracoord.df <- cbind(sort(degree(testgraph3)),
                      sort(betweenness(testgraph3)),
                      sort(closeness(testgraph3)),
                      sort(eigen_centrality(testgraph3)$vector))
buildPara <- function(tg, vnodes) {
  paracoord.df <- cbind((degree(tg)),
                        (betweenness(tg)),
                        (closeness(tg)),
                        (eigen_centrality(tg)$vector))
  colnames(paracoord.df) <- c("degree", "betweenness", "closeness", "eigen")
  paracoord.col <- rep("gold", vcount(tg))
  paracoord.col[vnodes] <- "red"
  paracoord.lty <- rep("2", vcount(tg))
  paracoord.lty[vnodes] <- "1"
  parcoord(paracoord.df, col = paracoord.col)
}

buildBox <- function(tg, vnodes) {
  paracoord.df <- cbind((degree(tg)),
                        (betweenness(tg)),
                        (closeness(tg)),
                        (eigen_centrality(tg)$vector))
  parachord.df.norm <- cbind(paracoord.df[,1]/max(paracoord.df[,1]),
                             paracoord.df[,2]/max(paracoord.df[,2]),
                             paracoord.df[,3]/max(paracoord.df[,3]),
                             paracoord.df[,4]/max(paracoord.df[,4]))
  colnames(paracoord.df) <- c("degree", "betweenness", "closeness", "eigen")
  boxplot(parachord.df.norm)
}

buildHist <- function(tg) {
  par(mfrow=c(2,2))
  hist(centralization.betweenness(tg)$res)
  hist(centralization.closeness(tg)$res)
  hist(centralization.degree(tg)$res)
  hist(centralization.evcent(tg)$vector)
}

buildAll <- function(tg) {
  par(mfrow=c(2,3))
  plot(tg)
  buildPara(tg, c(1,2,3,4,5,6,7,8,9,10))
  hist(centralization.betweenness(tg)$res)
  hist(centralization.closeness(tg)$res)
  hist(centralization.degree(tg)$res)
  hist(centralization.evcent(tg)$vector)
}

buildMini <- function(tg) {
  par(mfrow=c(1,3))
  plot(tg)
  buildPara(tg, c(1,2,3,4,5,6,7,8,9,10))
  buildBox(tg, c(1,2,3,4,5,6,7,8,9,10))
}

# Calculate Degree dist
plot(hist(degree(testgraph2), -1:max(degree(testgraph2)), plot = FALSE)$density)



# iGraph Permutation
fake.df <- as.data.frame(cbind(c("A","A","A","B","C","D","E"),
                 c("B","E","D","C","D","F","D"),
                 c(4,2,1,6,3,2,3)))
colnames(fake.df) <- c("to", "from", "weight")
fakeg <- graph.data.frame(fake.df, directed = F)
plot(fakeg)

colnames(fake.df)
colnames(prototypeMX)

# Dim1 --> Node Color --> V(graph)$color
# Dim3 --> Node Size  --> V(graph)$size

# Mapping input by user
# { "Dim1" : "Node Color",
#   "Dim2" : "Node Size" }

# Permutation function
# Takes in mapping, extracts keys, and returns permutations

# Lookup Table
# "Node Color" : "color"
# "Node Size"  : "size"

# Modify graph properties
# Report encoding risks: primarily if data type and encoding type do not match



