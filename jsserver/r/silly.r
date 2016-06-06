#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE);

library(RJSONIO)

args = toJSON(args);

write.table(args, 'savedoutput/test.json');
print("0");

