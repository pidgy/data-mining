#######################################################
# File and Directory setup 
#######################################################
dir <- "" # set me
rootdir <- paste(getwd(), dir, sep="/")
codedir <- "Code"
datadir <- "Data"
# Directories
code.dir <- paste(rootdir, codedir, sep="/")
data.dir <- paste(rootdir, datadir, sep="/")
# File constants 
file <- paste(data.dir, "cars.data", sep="/")
file.rows <- paste(data.dir, "cars.names", sep="/")
file.cols <- paste(data.dir, "cars.data.des", sep="/")
file <- paste(data.dir, "Online Retail.xlsx", sep="/")
# importedlibraries
library(readxl)
library(sets)
library(rggobi)
library(arules)
library(arulesViz)
######################################################