if (!exists("file.cars")) {
  print ("common.r needs to be sourced")
}
######################################################
# Scan the datasets and store the parsed data
# 
# d.cars | The table for vehicle metadata
# d.cols | The column names for vehicle metadata (vehicle measurment variables)
# d.rows | The row names for vehicle metadata (nehicle names) 
######################################################
d.cars           <- read.table(file.cars)
d.rows           <- noquote(scan(d.names, ""))
rexp             <- "^(\\w+)\\s?(.*)$" # regex expression to trim 'REAL n' values
d.cols           <- scan (file.cars.cols, what ="a", sep ="\n")
d.cols           <- sub(rexp,"\\1",d.cols) 
n.cols           <- length(d.rows)
n.rows           <- length(d.cols)
colnames(d.cars) <- d.cols
#rownames(d.cars) <- d.rows
######################################################

######################################################
# Examination
######################################################

# Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.
panel.smooth.asp <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                              cex = 1, col.smooth = "red", span = 2/3, iter = 3, asp,...) 
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex, asp=1)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth,...) 
}
## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)*r
  text(0.5, 0.5, txt, cex = cex.cor)
}
## put histograms on the diagonal
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

# Scatter plot matricies
pairs(d.cars, upper.panel=panel.cor, diag.panel=panel.hist)

g <- ggobi(d.cars)
display(g[1], "Scatterplot Matrix")

# inc (x=integer)
# Since r data types are immutable, 
# I have implemented my own increment function
# which increases a value x by y
incr <- function(x,y)
{
  if (is.null(x)){
    print(x)
    eval.parent(substitute(x <- 0))
  }
  eval.parent(substitute(x <- x + y))
}

# count.origin(x=table)
# Count the table$origin column for a given table
# if no such coloumn exists, the function will
# return in a clean manner, else return a matrix
count.origin <- function (t) 
{
  if (is.null(t$origin)) {
    return("Invalid Table")
  }
  origins <- matrix(c(0), ncol=3, byrow=TRUE)
  colnames(origins) <- c("American","Japanese","European")
  rownames(origins) <- c("count")
  for (i in t$origin) {
    if (is.na(i)) next
    if (i == 1) {
      incr(origins[1,1], 1)
    } else if (i == 2) {
      incr(origins[1,2], 1)
    } else if (i ==3 ) {
      incr(origins[1,3], 1)
    }
  }
  return (origins)
}
(origins <- count.origin(d.cars))

# threshold.byorigin
# Create a new table where all columns that = origin
# are evenly represented by only adding n rows of
# a specific origin for each variable
# return a new table with each variable in columns is 
# evenly represented
threshold.byorigin <- function(t, n) {
  col.thresh.count <- c(0,0,0)
  new.table <- matrix(ncol=ncol(t), byrow=TRUE)
  colnames(new.table) <- colnames(t)
  t <- t[sample(nrow(t)),]
  for (i in 1:nrow(t)) {
    o <- t[i,"origin"]
    if (is.na(o)) {
      next
    }
    if (col.thresh.count[o] == n) {
      next
    }
    col.thresh.count[o] <- col.thresh.count[o] + 1
    row <- t[i,1:ncol(t)]
    if (any(is.na(row))) {
      next
    }
    new.table <- merge(new.table, row, all=TRUE)
  }
  return (new.table)
}

# Get the lower bound amount of each origin
lower.bound <- 73
d.cars.t <- threshold.byorigin(d.cars, lower.bound)

# omit all rows with NA values
d.cars.t <- threshold.byorigin(d.cars.t, 68)
d.cars.t <- na.omit(d.cars.t)
(origins <- count.origin(d.cars.t))

# visualize the data
g <- ggobi(d.cars.t)
display(g[1], "Parallel Coordinates Display")
glyph_color(g[1]) <- c(rep(4,22))

# EOF
