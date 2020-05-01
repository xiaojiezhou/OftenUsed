
#  install.packages('ROracle')

require(ROracle)
exaDrv = dbDriver("Oracle")
exaCon = dbConnect(exaDrv, username="[AN_RT_WS95]", dbname="exa_uskrgprdh")

statsdat <- dbGetQuery(exaCon,
                       "
                       SELECT * FROM HHSPEND_MONTH_TEST_V2
                       where rownum <= 100
                       "
)

dbDisconnect(exaCon)

str(statsdat)