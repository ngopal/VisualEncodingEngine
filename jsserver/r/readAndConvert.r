#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE);

library(igraph)
library(RJSONIO)

# args[1] = file name
# args[2] = file format

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

graphdata <- read.graph(args[1], format=args[2]);

toCytoJSON(graphdata))