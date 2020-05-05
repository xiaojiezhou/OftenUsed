########################### Step0-data.R ##################### 
# File name:  Step0-data.R
# Purpose:  
# Population:  
########### Set up ########
rm(list=ls())
rm(list=c(''))

# path = "/nfs/science/users/xiaojiez/P2020/"
path = "/Users/x644435/Documents/Private/kaggle/M5"
setwd(paste(path))

#source("/nfs/science/users/xiaojiez//OftenUsed/RCode/FreqUsedRFunc.r")
source("/Users/x644435/Desktop/RCode/FreqUsedRFunc.r")

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









