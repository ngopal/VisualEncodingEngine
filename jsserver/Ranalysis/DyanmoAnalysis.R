library(randomForest)
library(RJSONIO)
library(ROCR)



#Converting HEX color to INT
rgbToInt <- function(red,green,blue) {
  # formula from https://www.shodor.org/stella2java/rgbint.html
  return(256*256*red+256*green+blue)
}
calcPerceivedBrightness <- function(red,green,blue) {
  # formula from http://stackoverflow.com/questions/596216/formula-to-determine-brightness-of-rgb-color
  # Another potential option http://stackoverflow.com/questions/12043187/how-to-check-if-hex-color-is-too-black
  return( (0.299*red + 0.587*green + 0.114*blue) )
}

cdat <- c()
cdatcombos <- c()
brightness <- c()
for (r in c(1,255)) {
  for (g in c(1,255)) {
    for (b in c(1,255)) {
      cdatcombos = append(cdatcombos, c(r,g,b))
      brightness = append(brightness, calcPerceivedBrightness(r,g,b))
      cdat = append(cdat, rgbToInt(r,g,b))
    }
  }
}
cbind(matrix(cdatcombos, 8, 3), cdat, brightness)

whichMeta <- function(jd) {
  for (t in 1:length(jd)) { 
    if(jd[[t]]$type=="meta") {
      return(t)
    } 
  }
}

# Dealing with Survey data
convertSurvey <- function(file) {
  tt <- tryCatch(  read.delim(file, header = F), error=c("PARSE ERROR"))
  tt <- as.character(unlist(tt))
  tt <- gsub("\\\\", "", tt)
  tt <- gsub(",", "\",\"", tt)
  tt <- gsub(":", "\":\"", tt)
  tt <- gsub("[{]", "{\"", tt)
  tt <- gsub("[}]", "\"}", tt)
  return(tt)
}

# substr(files[300], 1, 36)
# substr(surveyfiles[3], 1, 35)
# grep(substr(files[100], 1, 36), surveyfiles)

loadResultsDoubleEnc <- function(path) {
  files = list.files(path)[grep('survey',list.files(path), invert = T)]
  surveyfiles = list.files(path)[grep('survey',list.files(path), invert = F)]
  jnode <- c()
  for (f in files) {
    cat(paste(path,f,sep=''), '\n')
    jdat <- fromJSON(paste(path,f,sep=''))
    met <- whichMeta(jdat)
    surveyfile <- fromJSON(convertSurvey(paste(path,surveyfiles[grep(substr(f, 1, 36), surveyfiles)],sep='')))
    for (l in 1:length(jdat)) {
      if (jdat[[l]]$type == "node" || jdat[[l]]$type == "edge") {
        jnode = append(jnode, c(f, jdat[[l]]$id, jdat[[l]]$name, jdat[[l]]$Nencoding1, jdat[[l]]$Nencoding2, jdat[[l]]$Eencoding1, jdat[[l]]$Eencoding2, jdat[[l]]$results[1], jdat[[l]]$results[2], jdat[[l]]$results[3], jdat[[l]]$results[4], jdat[[l]]$position[1], jdat[[l]]$position[2], jdat[[met]]$browser, jdat[[met]]$clickX, jdat[[met]]$clickY, jdat[[met]]$windowwidth, jdat[[met]]$windowheight, jdat[[met]]$time, jdat[[l]]$selected, jdat[[l]]$participantResponse, surveyfile[1], surveyfile[2], surveyfile[3], surveyfile[4], surveyfile[5], surveyfile[6]   ))
      }
      else if (jdat[[l]]$type == "meta") {
        cat('found meta\n')
      }
      else {
        cat('ERROR\n')
      }
    }
  }
  return(jnode)
}

expd <- loadResultsDoubleEnc('~/Code/VisualEncodingEngine/jsserver/downloaded/')

expd.dat <- c()
for (i in 1:ceiling(length(expd)/21)) { #6740
  expd.dat <- rbind(expd.dat, matrix(expd[(1+(21*i)):(21+(21*i))], 1, 21))
}

expd <- data.frame(t(matrix(expd, 21, length(expd)/21))) 
# colnames(expd) <- c("file","id", "name", "encoding1", "encoding2", "nodecolor", "nodeshape", "nodeborder", "nodesize", "xpos", "ypos", "selected")
colnames(expd) <- c("file","id", "name", "nodecolor", "nodeshape", "nodeborder", "nodesize", "selected")
expd[,6] <- as.numeric(gsub('px','',expd[,6]))
expd[,7] <- as.numeric(gsub('px','',expd[,7]))
expd[,8] <- as.numeric(as.character(expd[,8]))

expd$nodecolor <- revalue(expd$nodecolor, c("#999"="#999999"))




### Let's try and connect through MONGO ####
## http://stackoverflow.com/questions/30738974/rjava-load-error-in-rstudio-r-after-upgrading-to-osx-yosemite
library(rJava)
library(RMongo)
library(plyr)
pilotdb <- mongoDbConnect('pilot')
query <- dbGetQuery(pilotdb, 'evaldata', '{}')
query <- dbGetQuery(pilotdb, 'evaldata', '{"page": {"$lt":10}}')
surveydata <- dbGetQuery(pilotdb, 'evaldata', '{ page: "survey" }')
collectedData <- dbGetQuery(pilotdb, 'evaldata', '{ "page": {"$ne":"survey"}  }')
collectedData <- dbGetQuery(pilotdb, 'evaldata', '{ "page": {"$ne":"survey"}, "user":"488238d8-99be-e65d-ebb8-ce7c04c92b25"  }')
dataObjs <- collectedData[c("dataObj")]
expd.dat <- c()
for (i in 1:dim(dataObjs)[1]) {
  jdat <- fromJSON(dataObjs[i,])
  met <- whichMeta(jdat)
  for (l in 1:length(jdat)) {
    if (jdat[[l]]$type != "meta") {
      expd.dat <- rbind(expd.dat, c(jdat[[l]]$id, jdat[[l]]$name, jdat[[l]]$Nencoding1, jdat[[l]]$Nencoding2, jdat[[l]]$Eencoding1, jdat[[l]]$Eencoding2, jdat[[l]]$results[1], jdat[[l]]$results[2], jdat[[l]]$results[3], jdat[[l]]$results[4], jdat[[l]]$results[5], jdat[[l]]$results[6], jdat[[l]]$results[7], jdat[[l]]$position[1], jdat[[l]]$position[2], jdat[[met]]$browser, jdat[[met]]$clickX, jdat[[met]]$clickY, jdat[[met]]$windowwidth, jdat[[met]]$windowheight, jdat[[met]]$time, jdat[[l]]$selected, jdat[[l]]$participantResponse))
    }
  }
}

colnames(expd.dat) <- c("id", "name", "Nencoding1", "Nencoding2", "Eencoding1", "Eencoding2", "background-color", "shape", "border-width", "height", "width", "line-color", "line-style", "xposition", "yposition", "browser", "clickx", "clicky", "windowwidth", "windowheight", "time", "selected", "participantResponse")
expd.dat[,9] <- as.numeric(gsub('px','',expd.dat[,9]))
expd.dat[,10] <- as.numeric(gsub('px','',expd.dat[,10]))
expd.dat[,11] <- as.numeric(gsub('px','',expd.dat[,11]))
expd.dat <- data.frame(expd.dat)
#replace "cy.js selection blue" with "normal gray"
expd.dat[,7] <- revalue(expd.dat[,7], c("#0169D9"="#999999"))
expd.dat[,7] <- revalue(expd.dat[,7], c("#999"="#999999"))
expd.dat[,12] <- revalue(expd.dat[,12], c("#0169D9"="#999999"))
expd.dat[,12] <- revalue(expd.dat[,12], c("#999"="#999999"))
#rgbtpint
tt <- makeRGBMat(expd.dat, 7)
expd.dat[,7] <- as.numeric(rgbToInt(tt[,1], tt[,2], tt[,3]))
tt2 <- makeRGBMat(expd.dat, 12)
expd.dat[,12] <- as.numeric(rgbToInt(tt2[,1], tt2[,2], tt2[,3]))
#brightness
expd.dat <- cbind(expd.dat, as.numeric(calcPerceivedBrightness(tt[,1], tt[,2], tt[,3])), as.numeric(calcPerceivedBrightness(tt2[,1], tt2[,2], tt2[,3])))
colnames(expd.dat) <- c(colnames(expd.dat)[c(-24,-25)],"nodeBrightness", "lineBrightness")

expd.nodes <- data.frame(expd.dat[which(expd.dat[,2] != "NA"),])
expd.nodes <- expd.nodes[which(as.numeric(as.character(expd.nodes[,22])) <= 1),]

expd.nodes[,7] <- as.numeric(as.character(expd.nodes[,7]))
expd.nodes[,9] <- as.numeric(as.character(expd.nodes[,9]))
expd.nodes[,10] <- as.numeric(as.character(expd.nodes[,10]))
expd.nodes[,11] <- as.numeric(as.character(expd.nodes[,11]))
expd.nodes[,12] <- as.numeric(as.character(expd.nodes[,12]))
expd.nodes[,14] <- as.numeric(as.character(expd.nodes[,14]))
expd.nodes[,15] <- as.numeric(as.character(expd.nodes[,15]))
expd.nodes[,17] <- as.numeric(as.character(expd.nodes[,17]))
expd.nodes[,18] <- as.numeric(as.character(expd.nodes[,18]))
expd.nodes[,19] <- as.numeric(as.character(expd.nodes[,19]))
expd.nodes[,20] <- as.numeric(as.character(expd.nodes[,20]))
expd.nodes[,21] <- as.numeric(as.character(expd.nodes[,21]))
expd.nodes[,22] <- as.numeric(as.character(expd.nodes[,22]))
expd.nodes[,24] <- as.numeric(as.character(expd.nodes[,24]))
expd.nodes[,25] <- as.numeric(as.character(expd.nodes[,25]))

tuneRF(x = expd.nodes[,c(-1, -2, -3, -4, -5, -6, -23)], y = expd.nodes[,c(22)], plot = T, doBest = T)
rf1 <- randomForest(selected ~ ., data=expd.nodes[,c(-1, -2, -3, -4, -5, -6, -23)], importance=TRUE, proximity=TRUE)
print(rf1)
rf1$importance
varImpPlot(rf1,type=2)
rf1.p <- classCenter(expd.nodes[,c(-1, -2, -3, -4, -5, -6, -23)], expd.nodes[,22], rf1$proximity)

tuneRF(x = expd.nodes[,c(-1, -2, -3, -4, -5, -6, -12, -13, -23, -25)], y = expd.nodes[,c(22)], plot = T, doBest = T)
rf1.nodeOnly <- randomForest(selected ~ ., data=expd.nodes[,c(-1, -2, -3, -4, -5, -6, -12, -13, -23, -25)], importance=TRUE, proximity=TRUE)
print(rf1.nodeOnly)
rf1.nodeOnly$importance
varImpPlot(rf1.nodeOnly,type=2)

rf1.nodeOnly.timedep <- randomForest(time ~ ., data=expd.nodes[,c(-1, -2, -3, -4, -5, -6, -12, -13, -23, -25)], importance=TRUE, proximity=TRUE)
print(rf1.nodeOnly.timedep)
rf1.nodeOnly.timedep$importance
varImpPlot(rf1.nodeOnly.timedep,type=2)

rf1.nodeOnly.timedep.selOnly <- randomForest(time ~ ., data=expd.nodes[which(expd.nodes[,22] == 1),c(-1, -2, -3, -4, -5, -6, -12, -13, -23, -25)], importance=TRUE, proximity=TRUE)
print(rf1.nodeOnly.timedep.selOnly)
rf1.nodeOnly.timedep.selOnly$importance
varImpPlot(rf1.nodeOnly.timedep.selOnly,type=2)

expd.edges <- data.frame(expd.dat[which(expd.dat[,2] == "NA"),])
expd.edges <- expd.edges[which(as.numeric(as.character(expd.edges[,22])) <= 1),]

expd.edges[,7] <- as.numeric(as.character(expd.edges[,7]))
expd.edges[,9] <- as.numeric(as.character(expd.edges[,9]))
expd.edges[,10] <- as.numeric(as.character(expd.edges[,10]))
expd.edges[,11] <- as.numeric(as.character(expd.edges[,11]))
expd.edges[,12] <- as.numeric(as.character(expd.edges[,12]))
expd.edges[,17] <- as.numeric(as.character(expd.edges[,17]))
expd.edges[,18] <- as.numeric(as.character(expd.edges[,18]))
expd.edges[,19] <- as.numeric(as.character(expd.edges[,19]))
expd.edges[,20] <- as.numeric(as.character(expd.edges[,20]))
expd.edges[,21] <- as.numeric(as.character(expd.edges[,21]))
expd.edges[,22] <- as.numeric(as.character(expd.edges[,22]))
expd.edges[,24] <- as.numeric(as.character(expd.edges[,24]))
expd.edges[,25] <- as.numeric(as.character(expd.edges[,25]))

tuneRF(x = expd.edges[,c(-1, -2, -3, -4, -5, -6, -14, -15, -23)], y = expd.edges[,c(22)], plot = T, doBest = T)
rf2 <- randomForest(selected ~ ., data=expd.edges[,c(-1, -2, -3, -4, -5, -6, -14, -15, -23)], importance=TRUE, proximity=TRUE)
print(rf2)
rf2$importance
varImpPlot(rf2,type=2)



# Let's use an RF
makeRGBMat <- function(dat,col) {
  voodoo <- c()
  for (i in 1:dim(dat)[1]) {
    if (dat[i,col] == "#999") {
      voodoo <- append(voodoo, t(col2rgb("#999999")))
    }
    else {
      voodoo <- append(voodoo, t(col2rgb(dat[i,col])))
    }
  }
  mycolors <- t(matrix(voodoo, 3, length(voodoo)/3))
  return(mycolors)
}

library(randomForest)
expd <- data.frame(cbind(expd[,1:3],mycolors,expd[5:8]))
colnames(expd) <- colnames(expd) <- c("file","id", "name", "R","G","B", "nodeshape", "nodeborder", "nodesize", "selected")
expd.noname <- expd[,c(4:10)]
tuneRF(x = expd.noname[,-7], y = expd.noname[,7], plot = T, doBest = T)
rf1 <- randomForest(selected ~ ., data=expd.noname, importance=TRUE, proximity=TRUE, mtry = 1)
print(rf1)
rf1$importance
varImpPlot(rf1,type=2)

# NOTE THAT THE TREE IS UNBALANCED RIGHT NOW, AND MUST BE SAMPLED
# BALANCED BEFORE RESULTS ARE RELIABLE

rf1.perf = performance(  prediction(labels = expd.noname$selected, predictions = rf1$predicted)  ,"tpr","fpr")

#plot the curve
plot(rf1.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
lines(unlist(rf1.perf@x.values),unlist(rf1.perf@y.values), col=4, lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#compute area under curve

auc.rf1 <- performance(    prediction(labels = expd.noname$selected, predictions = rf1$predicted)   ,"auc")
auc.rf1 <- unlist(slot(auc.rf1, "y.values"))

minauc<-min(round(auc.rf1, digits = 2))
maxauc<-max(round(auc.rf1, digits = 2))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
minauct
maxauct

# EVEN THOUGH THE TREE IS UNBALANCED, THERE IS 0.89 AUC

# Prototypes below
rf1.p <- classCenter(expd.noname[,-7], expd.noname[,7], rf1$proximity)








# Future Functions Below



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
  if (expd[i,4] == "#999") {
    voodoo <- append(voodoo, t(col2rgb("#999999")))
  }
  else {
    voodoo <- append(voodoo, t(col2rgb(expd[i,4])))
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



# Threshold that Neil provided
gthresh <- function(numNodes) { return(  ceiling(dim(combn(1:numNodes, 2))[2]*(log(numNodes)/numNodes))  ) }
plot(10:300, unlist(lapply(10:300, FUN=gthresh)), type="l")

