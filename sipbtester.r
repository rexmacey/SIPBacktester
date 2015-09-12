source("smwscreen.r")
source("sipbconfig.r")
source("sipbutils.r")

screening.fun <- smw  
passing.lst <- lapply(sipbInstallDates,screening.fun)
port.wts<-lapply(passing.lst,port_wts,method="eq",maxwt=0.60)
save(passing.lst,port.wts,file="temp.rdata")

