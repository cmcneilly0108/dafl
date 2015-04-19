### Presentation to Chicago R-User's Group (CRUG) on May 1, 2013
### Peter Carl
### Favorite Packages: xlsx
 
### Overview
# Package: xlsx
# Type: Package
# Title: Read, write, format Excel 2007 and Excel 97/2000/XP/2003 files
# Version: 0.5.0
# Date: 2012-09-23
# Depends: xlsxjars, rJava
# Author and Maintainer: Adrian A. Dragulescu <adrian.dragulescu@gmail.com>
# License: GPL-3
# URL: http://code.google.com/p/rexcel/
 
### Installation
# Get it from CRAN
# install.packages(xlsx)
 
### Preparing the workspace
require(PerformanceAnalytics)
require(xlsx)
 
### Reading from an Excel worksheet
# Download the file using wget
url <- "http://www.djindexes.com/mdsidx/downloads/xlspages/ubsci_public/DJUBS_full_hist.xls"
system(paste('wget ', url))
 
# Read in the workbook data on the second sheet
# x = read.xlsx("DJUBS_full_hist.xls", sheet="sheet"Total Return", stringsAsFactors=FALSE) # Too slow for big spreadsheets
x <- read.xlsx2("DJUBS_full_hist.xls", sheetName="Total Return", header=TRUE, startRow=3, as.data.frame=TRUE, stringsAsFactors=FALSE, colClasses=c("Date", rep("numeric", 100)))
# The read.xlsx2 function does more work in Java so it achieves better performance (an order of magnitude faster on sheets with 100,000 cells or more). Much faster, but dates come in as numeric unless specified in colClasses.
 
# Or the result can be fixed with this...
# excelDate2Date <- function(excelDate) { # originally from HFWutils pkg, now abandoned
# Date <- excelDate + as.Date("1900-01-01") - 2
# ## FIXME: add "if >1900-Feb-28" switch?
# return(Date)
# }
 
# Read the more descriptive headings from a specific sheet
x.tags <- read.xlsx2("DJUBS_full_hist.xls", sheetName="Total Return", header=FALSE, startRow=1, endRow=3, as.data.frame=TRUE, stringsAsFactors=FALSE)
 
# head(x, n=10) # get a sense of what we've read in
# tail(x, n=10) # the author has some notes at the end of the data
#
# Comes in as a mix of classes in a data.frame
# > class(x)
# [1] "data.frame"
# > class(x[2,2])
# [1] "numeric"
# > class(x[1,1])
# [1] "Date"
 
### Parsing the data
# Everything was read in as a string, except for a few NA's at the end
# x = na.omit(x)
 
# Get rid of the last two lines, which contains the disclaimer
x = x[-which(is.na(x[,1])),]
 
# Remove blank columns between sections for both the data and the tags
x = x[,-which(lapply(x,function(x)all(is.nan(x)))==TRUE)]
x.tags = x.tags[,-which(apply(x.tags,2,function(x)all(x=="")))]
 
# Parse the dates, remembering that Excel does not keep track of time zones and DST
x.ISOdates = x[,1]
 
# Convert data into a time series of prices
x.P=as.xts(x[-1], order.by=x.ISOdates)
 
# Rename the columns using something more descriptive
colnames(x.P) = x.tags[2,-1]
 
# Use the descriptive data to identify subsets
# > unique(as.character(x.tags[1,]))
# [1] "" "Currency" "Subindex" "Individual Commodities"
# [5] "Additional Commodities"
 
# Use subsetting to get a vector of column names
# > as.character(x.tags[2, which(x.tags[1,]=="Subindex")])
# [1] "Agriculture" "Energy" "ExEnergy" "Grains" "Industrial Metals"
# [6] "Livestock" "Petroleum" "Precious Metals" "Softs" "Composite Crude"
# [11] "Composite Wheat"
x.subindexes = as.character(x.tags[2, which(x.tags[1,]=="Subindex")])
 
# > as.character(x.tags[2, grep("Commodities", x.tags[1,])])
# [1] "Aluminum" "Brent Crude" "Coffee" "Copper (COMEX)" "Corn"
# [6] "Cotton" "Gold" "Heating Oil" "Kansas Wheat" "Lean Hogs"
# [11] "Live Cattle" "Natural Gas" "Nickel" "Silver" "Soybeans"
# [16] "Soybean Meal" "Soybean Oil" "Sugar" "Unleaded Gasoline" "Wheat"
# [21] "WTI Crude Oil" "Zinc" "Cocoa" "Lead" "Platinum"
# [26] "Tin"
x.commodities = as.character(x.tags[2, grep("Commodities", x.tags[1,])])
 
# Calculate returns from prices
x.R = Return.calculate(x.P[,x.commodities])
 
# --- Slide 0 ---
# > head(x.R)
# Aluminum Brent Crude Coffee Copper (COMEX) Corn Cotton Gold
# 1991-01-02 NA NA NA NA NA NA NA
# 1991-01-03 0.0110040000 -0.045238000 0.0138090000 -0.024966000 0.002338000 0.013373000 -0.005445000
# 1991-01-04 0.0004599388 -0.058984333 -0.0037413359 -0.003259374 0.006639477 -0.002423589 -0.004190819
# 1991-01-07 0.0060614809 0.150057989 0.0174145756 0.008306786 0.008027806 -0.007552549 0.023785651
# 1991-01-08 -0.0166027909 -0.026213992 0.0007347181 -0.019509577 -0.011495507 -0.003766638 -0.009661283
# 1991-01-09 -0.0055101154 0.008863234 -0.0031341165 -0.008988240 -0.004114776 -0.002593289 0.001912069
# ...
 
### Analyzing the data
# --- Slide 1 ---
# Create a table of summary statistics
x.AnnRet = t(table.AnnualizedReturns(x.R), Rf=0.3/12)
x.RiskStats = as.data.frame(t(table.RiskStats(x.R)))
# > x.RiskStats
# Annualized Return Annualized Std Dev Annualized Sharpe Ratio Annualized Downside Deviation
# Aluminum -0.0110 0.2022 -0.0542 0.1433
# Brent Crude 0.1233 0.3080 0.4002 0.2156
# Coffee -0.0403 0.3745 -0.1075 0.2551
# Copper (COMEX) 0.0909 0.2690 0.3379 0.1873
# Corn -0.0387 0.2538 -0.1525 0.1769
# ...
 
### Writing the resulting table to an Excel worksheet
# --- Slide 2 ---
# Create a new workbook for outputs
outwb <- createWorkbook()
 
# Define some cell styles within that workbook
csSheetTitle <- CellStyle(outwb) + Font(outwb, heightInPoints=14, isBold=TRUE)
csSheetSubTitle <- CellStyle(outwb) + Font(outwb, heightInPoints=12, isItalic=TRUE, isBold=FALSE)
csTableRowNames <- CellStyle(outwb) + Font(outwb, isBold=TRUE)
csTableColNames <- CellStyle(outwb) + Font(outwb, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
csRatioColumn <- CellStyle(outwb, dataFormat=DataFormat("0.0")) # ... for ratio results
csPercColumn <- CellStyle(outwb, dataFormat=DataFormat("0.0%")) # ... for percentage results
 
# --- Slide 3 ---
# Which columns in the table should be formatted which way?
RiskStats.colRatio = list(
 '3'=csRatioColumn,
 '5'=csRatioColumn,
 '8'=csRatioColumn,
 '15'=csRatioColumn)
RiskStats.colPerc =list(
 '1'=csPercColumn,
 '2'=csPercColumn,
 '4'=csPercColumn,
 '6'=csPercColumn,
 '7'=csPercColumn,
 '9'=csPercColumn,
 '10'=csPercColumn,
 '13'=csPercColumn,
 '14'=csPercColumn)
 
# --- Slide 4 ---
# Create a sheet in that workbook to contain the table
sheet <- createSheet(outwb, sheetName = "Performance Table")
 
# Add the table calculated above to the sheet
addDataFrame(x.RiskStats, sheet, startRow=3, startColumn=1, colStyle=c(RiskStats.colPerc,RiskStats.colRatio), colnamesStyle = csTableColNames, rownamesStyle=csTableRowNames)
setColumnWidth(sheet,colIndex=c(2:15),colWidth=11)
setColumnWidth(sheet,colIndex=16,colWidth=13)
setColumnWidth(sheet,colIndex=17,colWidth=6)
setColumnWidth(sheet,colIndex=1,colWidth=0.8*max(length(rownames(x.RiskStats))))
 
# --- Slide 5 ---
# Create the Sheet title ...
rows <- createRow(sheet,rowIndex=1)
sheetTitle <- createCell(rows, colIndex=1)
setCellValue(sheetTitle[[1,1]], "Ex-Post Returns and Risk")
setCellStyle(sheetTitle[[1,1]], csSheetTitle)
# ... and subtitle
rows <- createRow(sheet,rowIndex=2)
sheetSubTitle <- createCell(rows,colIndex=1)
setCellValue(sheetSubTitle[[1,1]], "Since Inception")
setCellStyle(sheetSubTitle[[1,1]], csSheetSubTitle)
 
### Add a chart to a different sheet
# --- Slide 6 ---
# Construct the chart as a dib, emf, jpeg, pict, png, or wmf file.
require(gplots)
skewedG2R20 = c(colorpanel(16, "darkgreen","yellow"), colorpanel(5, "yellow", "darkred")[-1])
png(filename = "corr.jpeg", width = 6, height = 8, units = "in", pointsize=12, res=120)
require(PApages)
page.CorHeatmap(x.R[,x.commodities], Colv=TRUE, breaks = seq(-1,1,by=.1), symkey=TRUE, col=skewedG2R20, tracecol="darkblue", cexRow=0.9, cexCol=0.9)
dev.off()
 
# --- Slide 7 ---
# Create a sheet in that workbook to contain the graph
sheet <- createSheet(outwb, sheetName = "Correlation Chart")
 
# Create the Sheet title and subtitle
rows <- createRow(sheet,rowIndex=1)
sheetTitle <- createCell(rows, colIndex=1)
setCellValue(sheetTitle[[1,1]], "Correlations Among Commodities")
setCellStyle(sheetTitle[[1,1]], csSheetTitle)
rows <- createRow(sheet,rowIndex=2)
sheetSubTitle <- createCell(rows,colIndex=1)
setCellValue(sheetSubTitle[[1,1]], "Correlations of daily returns since inception")
setCellStyle(sheetSubTitle[[1,1]], csSheetSubTitle)
 
# Add the file created previously
addPicture("corr.jpeg", sheet, scale = 1, startRow = 4, startColumn = 1)
 
# --- Slide 8 ---
# Save the workbook to a file...
saveWorkbook(outwb, "DJUBS Commodities Performance Summary.xlsx")
 
# --- Slides 9, 10 ---
# Show screen captures of the resulting workbook