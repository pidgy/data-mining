wd <- ""
source(paste(wd, "Common.R", sep="/"))
source("Common.R")
######################################################
# d.retail Class
#
# get.<property> will return a column value for a specified row
######################################################
d.retail <- read_xlsx(file)
######################################################
#constants
COUNTRY <- "Country"
CUSTOMER.ID <- "CustomerID"
DESCRIPTION <- "Description"
INVOICE.NO <- "InvoiceNo"
INVOICE.DATE <- "InvoiceDate"
QUANTITY <- "Quantity"
STOCK.CODE <- "StockCode"
UNIT.PRICE <- "UnitPrice"
# getters for retail properties at row <r> or rows<ri:ri+n>
get.invoice     <- function(r,d=d.retail)     {d[r,"InvoiceNo"]}
get.stockcode   <- function(r,d=d.retail)     {d[r,"StockCode"]}
get.description <- function(r,d=d.retail)     {d[r,"Description"]}
get.quantity    <- function(r,d=d.retail)     {d[r,"Quantity"]}
get.invoicedate <- function(r,d=d.retail)     {d[r,"InvoiceDate"]}
get.unitprice   <- function(r,d=d.retail)     {d[r,"UnitPrice"]}
get.customerid  <- function(r,d=d.retail)     {d[r,"CustomerID"]}
get.country     <- function(r,d=d.retail)     {d[r,"Country"]}
get.all         <- function(r,d=d.retail)     {d[r,]}
get.which       <- function(r,w,d=d.retail)   {d[r, w]}
######################################################
# end class d.retail
######################################################

# Create the transactions for InvoiceNo and Description
transactions.invoice.description <- as(split(d.retail$Description, d.retail$InvoiceNo), "transactions")
# view the summary of the invoice description transactions
summary(transactions.invoice.description)
# generate all rules
rules.invoice.description <- apriori(transactions.invoice.description, parameter = list(sup = 0.01, conf = 0.5, target="rules"))
# summarize the rules
summary(rules.invoice.description)
# inspect the rules
inspect(rules.invoice.description)
# sort lift values in decreasing order
top.lift <- sort(rules.invoice.description, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 10)) 

plot(rules, main="Retail Item Associations | InvoiceNo -> Description", method = NULL, measure = "support", shading = "lift", interactive = FALSE, data = NULL, control = NULL, engine = "default")
plot(rules, main="Retail Item Associations | InvoiceNo -> Description", measure=c("support", "lift"), shading="confidence")
plot(rules, shading="order", control=list(main = "Two-key plot"))

transactions <- as(split(d.retail$Description, d.retail$CustomerID), "transactions")
rules<- apriori(transactions, parameter = list(sup = 0.06, conf = 0.5, target="rules"))
# generate all rules
# summarize the rules
summary(rules)
# inspect the rules
inspect(rules)

top.lift <- sort(rules, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 10)) 

#EOF