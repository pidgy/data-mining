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
file.cars       <- paste(data.dir, "cars.data",          sep="/")
file.cars.rows  <- paste(data.dir, "cars.names",         sep="/")
file.cars.cols  <- paste(data.dir, "cars.data.des",      sep="/")
file.retail     <- paste(data.dir, "Online Retail.xlsx", sep="/")
# Imported libraries
library(readxl)
library(sets)
library(rggobi)
library(arules)
library(arulesViz)
######################################################
