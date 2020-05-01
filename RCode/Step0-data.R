
########################### Step0-data.R ##################### 
# File name:  Step0-data.R
# Path: /nfs/science/kroger/xiaojiez/LCM/R/V1/
# Purpose:  data for feature representation
# Population:  
########### Set up ########
rm(list=ls())
rm(list=c(''))

# path = "/nfs/science/kroger/xiaojiez/LCM/R/V1/"
path = "/Users/x644435/Desktop/Other/Kaggle/Instacart"
setwd(paste(path))

#source("/nfs/science/kroger/xiaojiez/Rcode/FreqUsedRFunc.r")
source("/Users/x644435/Desktop/RCode/FreqUsedRFunc.r")

library(ggplot2)
library(dplyr)
library(reshape2)
library(strucchange)
library(data.table)
options(stringsAsFactors=FALSE)
####### Read all csv files from the directory #########
datapath="/Users/x644435/Desktop/Other/Kaggle/Instacart/Rawdata/"

filenames = filenames[grepl('.csv$',  dir(datapath))] # ends with csv
rnames = gsub("*.csv$", "", filenames)

print("Following files are to be read into R:")
print(filenames)

for (i in 1:length(filenames)) assign(rnames[i], read.csv(paste0(datapath,filenames[i])))


####### Read data in feather ########
read_feather(filename, columns = NULL)
write_feather(x, filename)

####### Read/Write from/to Oracle #########
require(ROracle)
exaDrv = dbDriver("Oracle")
exaCon = dbConnect(exaDrv, username="[AN_RT_WS95]", dbname="exa_uskrgprdh")

statsdat <- dbGetQuery(exaCon, 
                       "
                       SELECT * FROM LCM2014Q1_DE_AB
                       "
)


#-- WRITE TO ORACLE
# dbWriteTable(exaCon, "OCR_L3_20", clust_dat)  # to write data to Oracle database

dbDisconnect(exaCon) 

#########load/save R data#######
#  load(paste(path,"statsdat.RData", sep="") )

#  save(statsdat, file="statsdat.RData", compress=TRUE)









