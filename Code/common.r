#######################################################
# File and Directory setup 
#######################################################
dir.root   <- "." # set me
dir.code   <- "Code"
dir.data   <- "Data"
dir.cars   <- "cars"
dir.retail <- "retail"
# Directories
code.dir   <- paste(dir.root, dir.code,             sep="/")
data.dir   <- paste(dir.root, dir.data,             sep="/")
# File constants 
file       <- paste(data.dir, "cars.data",          sep="/")
file.rows  <- paste(data.dir, "cars.names",         sep="/")
file.cols  <- paste(data.dir, "cars.data.des",      sep="/")
file       <- paste(data.dir, "Online Retail.xlsx", sep="/")
# Imported libraries
library(readxl)
library(sets)
library(rggobi)
library(arules)
library(arulesViz)
######################################################
