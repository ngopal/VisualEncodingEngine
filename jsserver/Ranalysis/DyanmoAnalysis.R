library(randomForest)
library(RJSONIO)
library(ROCR)

# Research Questions
# 1. What are the ranked importances of node encodings?
# 2. What are the ranked importances of edge encodings?
# 3. How important is network structure to noticeability?
# 3a. Where do participants tend to click?


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

### Connecting through MONGO ####
## http://stackoverflow.com/questions/30738974/rjava-load-error-in-rstudio-r-after-upgrading-to-osx-yosemite
library(rJava)
library(RMongo)
library(plyr)
pilotdb <- mongoDbConnect('pilot')
dbGetQuery(pilotdb, 'evaldata', '{}')['X_id']

connectSurveyToClickData <- function() {
  uniqueSessions <- unlist(unique(dbGetQuery(pilotdb, 'evaldata', '{}')['user']))
  connectedData <- c()
  for (u in uniqueSessions) {
    cat(u,'\n')
    cldata <- dbGetQuery(pilotdb, 'evaldata', paste('{ user : "',u,'", "page" : { "$ne" : "survey"} }', sep=''))
    sudata <- dbGetQuery(pilotdb, 'evaldata', paste('{ user : "',u,'", "page" : "survey" }', sep=''))
    if (dim(sudata)[1] == 0 || dim(sudata)[2] == 0) {
      next
    }
    else {
      sudata <- sudata[c("question1", "question2", "question3", "question4", "question5", "question6")]
    }
    combinedData <- cbind(cldata, sudata)
    connectedData <- rbind(connectedData, combinedData)
  }
  return(  data.frame(connectedData)  )
}


surveydata <- dbGetQuery(pilotdb, 'evaldata', '{ page: "survey" }')

# Survey Data
survey.df <- data.frame(surveydata[c("question1", "question2", "question3", "question4", "question5", "question6")])
barplot(survey.df)
barplot(table(survey.df[,1]))
barplot(table(survey.df[,2]))
barplot(table(survey.df[,3]))
barplot(table(survey.df[,4]))
barplot(table(survey.df[,5]))

# Click Data
collectedData <- dbGetQuery(pilotdb, 'evaldata', '{ "page": {"$ne":"survey"}  }')
#collectedData <- dbGetQuery(pilotdb, 'evaldata', '{ "page": {"$ne":"survey"}, "user":"488238d8-99be-e65d-ebb8-ce7c04c92b25"  }')
#expd.dat <- data.frame(collectedData[names(head(collectedData))])
expd.dat <- connectSurveyToClickData()
expd.dat[,9] <- as.numeric(gsub('px','',expd.dat[,9]))
expd.dat[,10] <- as.numeric(gsub('px','',expd.dat[,10]))
expd.dat[,28] <- as.numeric(gsub('px','',expd.dat[,28]))

#replace "cy.js selection blue" with "normal gray"
expd.dat[,5] <- revalue(expd.dat[,5], c("#0169D9"="#999999"))
expd.dat[,5] <- revalue(expd.dat[,5], c("#999"="#999999"))
expd.dat[,15] <- revalue(expd.dat[,15], c("#0169D9"="#999999"))
expd.dat[,15] <- revalue(expd.dat[,15], c("#999"="#999999"))
#rgbtpint
tt <- makeRGBMat(expd.dat, 5)
expd.dat[,5] <- as.numeric(rgbToInt(tt[,1], tt[,2], tt[,3]))
tt2 <- makeRGBMat(expd.dat, 15)
expd.dat[,15] <- as.numeric(rgbToInt(tt2[,1], tt2[,2], tt2[,3]))
#brightness
expd.dat <- cbind(expd.dat, as.numeric(calcPerceivedBrightness(tt[,1], tt[,2], tt[,3])), as.numeric(calcPerceivedBrightness(tt2[,1], tt2[,2], tt2[,3])))
colnames(expd.dat) <- c(colnames(expd.dat)[c(-29,-30)],"nodeBrightness", "lineBrightness")



# Sampling Idea
# I suppose I can have up to 6 nodes without having to use SMOTE
# I will put this on hold because I don't think I need to balance classes yet
#dbGetQuery(pilotdb, 'evaldata', '{ "page": {"$ne":"survey"}, "selected":0, "network":"rn2", "name" : {"$ne" : "NA"}  }')

# Click Map
Xcoord <- expd.dat[,6]/expd.dat$windowwidth
Ycoord <- expd.dat[,7]/expd.dat$windowheight
clickX <- expd.dat[,1]/expd.dat$windowwidth
clickY <- expd.dat[,2]/expd.dat$windowheight
plot(Xcoord, Ycoord)
points(clickX, clickY, col="red")

expd.nodes <- data.frame(expd.dat[which(!is.na(expd.dat[,6])),])
expd.nodes <- expd.nodes[which(as.numeric(as.character(expd.nodes[,13])) <= 1),]
expd.edges <- data.frame(expd.dat[which(is.na(expd.dat[,6])),])
expd.edges <- expd.edges[which(as.numeric(as.character(expd.edges[,13])) <= 1),]

# Node Encodings Only Model / Selection
expd.nodes.1 <- data.frame(expd.nodes[,c(4,9,10,13,15,28,29)])
expd.nodes.1[,1] <- as.factor(expd.nodes.1[,1])
expd.nodes.1[,4] <- as.factor(expd.nodes.1[,4])

# I could consider using "network" as strata below
selectedPrevalence.nodes.1 <- sum(as.numeric(expd.nodes.1$selected))/length(as.numeric(expd.nodes.1$selected))
unselectedPrevalence.nodes.1 <- 100-sum(as.numeric(expd.nodes.1$selected))/length(as.numeric(expd.nodes.1$selected))
tuneRF(x = expd.nodes.1[,c(-3, -4, -7)], y = expd.nodes.1[,4], importance=TRUE, proximity=TRUE, classwt = c(selectedPrevalence.nodes.1, unselectedPrevalence.nodes.1))
rf1.nodes.1 <- randomForest(selected ~ ., data=expd.nodes.1[,c(-3, -7)], importance=TRUE, proximity=TRUE, classwt = c(selectedPrevalence.nodes.1, unselectedPrevalence.nodes.1))
print(rf1.nodes.1)
rf1.nodes.1$importance
varImpPlot(rf1.nodes.1,type=2)
abline(v = abs(min(rf1.nodes.1$importance[,4])), lty="longdash", lwd=2)
rf1.nodes.1.p <- classCenter(expd.nodes.1[-4], expd.nodes.1[,4], rf1.nodes.1$proximity)

# Node Encodings Only Model / Reaction Time
expd.nodes.2 <- data.frame(expd.nodes[,c(4,9,10,14,15,28,29)])
expd.nodes.2[,1] <- as.factor(expd.nodes.1[,1])
expd.nodes.2[,4] <- as.factor(expd.nodes.1[,4])

rf1.nodes.2 <- randomForest(reactionTime ~ ., data=expd.nodes.2, importance=TRUE, proximity=TRUE)
print(rf1.nodes.2)
rf1.nodes.2$importance
varImpPlot(rf1.nodes.2,type=2)
rf1.nodes.2.p <- classCenter(expd.nodes.1[-4], expd.nodes.1[,4], rf1.nodes.2$proximity)

# Edge Encodings Only Model / Selection
expd.edges.1 <- data.frame(expd.edges[,c(5, 13, 20, 30)])
expd.edges.1[,3] <- as.factor(expd.edges.1[,3])

rf1.edges.1 <- randomForest(selected ~ ., data=expd.edges.1, importance=TRUE, proximity=TRUE)
print(rf1.edges.1)
rf1.edges.1$importance
varImpPlot(rf1.edges.1,type=2)
rf1.edges.1.p <- classCenter(expd.edges.1[-2], expd.edges.1[,2], rf1.edges.1$proximity)

# Edge Encodings Only Model / Reaction Time
expd.edges.2 <- data.frame(expd.edges[,c(5, 14, 20, 30)])
expd.edges.2[,3] <- as.factor(expd.edges.2[,3])

rf1.edges.2 <- randomForest(reactionTime ~ ., data=expd.edges.2, importance=TRUE, proximity=TRUE)
print(rf1.edges.2)
rf1.edges.2$importance
varImpPlot(rf1.edges.2,type=2)
abline
rf1.edges.2.p <- classCenter(expd.edges.1[-2], expd.edges.1[,2], rf1.edges.2$proximity)

# Node and Edge Encodings Only Model / Selection
expd.both <- data.frame(expd.dat[which(!is.na(expd.dat[,6])),])
expd.both <- expd.nodes[which(as.numeric(as.character(expd.nodes[,13])) <= 1),]
expd.both <- expd.both[,-c(3, 16, 17, 18, 19, 21, 22, 24, 26, 27)]

expd.both[,3] <- as.factor(expd.both[,3])
expd.both[,7] <- as.factor(expd.both[,7])
expd.both[,10] <- as.factor(expd.both[,10])
expd.both[,15] <- as.factor(expd.both[,15])

tuneRF(expd.both[-12], expd.both[,12], plot = T)
rf.both.1 <- randomForest(selected ~ ., data=expd.both, importance=TRUE, proximity=TRUE)
print(rf.both.1)
rf.both.1$importance
varImpPlot(rf.both.1,type=2)
rf.both.1.p <- classCenter(expd.both[-12], expd.both[,12], rf.both.1$proximity)

# negative value means the mean error is larger than the variance of the response
# y. This could be because the predictor performs really poorly but also
# because of some calibration issue.

# Node and Edge Encodings Only Model / Reaction Time
rf.both.2 <- randomForest(reactionTime ~ ., data=expd.both, importance=TRUE, proximity=TRUE)
print(rf.both.2)
rf.both.2$importance
varImpPlot(rf.both.2,type=2)
rf1.edges.2.p <- classCenter(rf.both.1[-13], expd.edges.1[,13], rf.both.1$proximity)

# Try to attach demographic information to the DF and see how that affects selection
# cbind(expd.both, surveydata)


# NOTE THAT THE TREE IS UNBALANCED RIGHT NOW, AND MUST BE SAMPLED
# BALANCED BEFORE RESULTS ARE RELIABLE

rf1.perf = performance(  prediction(labels = expd.both$selected, predictions = rf.both.1$predicted)  ,"tpr","fpr")

#plot the curve
plot(rf1.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
lines(unlist(rf1.perf@x.values),unlist(rf1.perf@y.values), col=4, lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#compute area under curve

auc.rf1 <- performance(    prediction(labels = expd.both$selected, predictions = rf.both.1$predicted)   ,"auc")
auc.rf1 <- unlist(slot(auc.rf1, "y.values"))

minauc<-min(round(auc.rf1, digits = 2))
maxauc<-max(round(auc.rf1, digits = 2))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
minauct
maxauct


# RF UTILITIES
library(rfUtilities)

multi.collinear(expd.nodes.1[,2:7])
# This shows that I can remove nodeheight from the model since it mirrors nodewidth

multi.collinear(expd.edges.1[,-3])
# No multicollinearity

multi.collinear(expd.both[,c(-3, -7, -10, -15)])
# In addition to nodeheight, windowheight, windowwidth, and nodeBrightness
# may be removed due to collinearity


# RF UTILITIES CLASS BALANCE
# https://cran.r-project.org/web/packages/rfUtilities/rfUtilities.pdf
rf.nodes.1.balanced <- rf.classBalance(ydata = expd.nodes.1[,4], xdata = expd.nodes.1[,c(2,3,5,6)])






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

