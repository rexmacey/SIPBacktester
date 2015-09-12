#Explore / Build returns
setwd("C:/Users/Rex/Documents/Quant Trading/SIPBacktester")
source("smwscreen.r")
source("sipbconfig.r")
source("sipbutils.r")
rerun<-F
#creates dataframe with most recent "monthly" return for all companies using each SIP installation
#Not all companies have data for the full month.  

GetPrice<-function(CID,InstallDt,MNum=1){
    var1<-sprintf("PRICE_M%03d",MNum)
    var2<-sprintf("PRICE_M%03d",MNum+1)
    price_filename<-paste(mainfolder,"/rdata/sip_prices_",InstallDt,".rdata",sep="")
    load(price_filename)  #price_data is variable name
    x <- subset(price_data,COMPANY_ID==CID)
    if (nrow(x)==0){
        return(NA)
    } else {
        return(c(as.numeric(x[1,var1]),as.numeric(x[1,var2])))
    }
}

generate_prices<-function(){
    #sink(file="generate_prices_log.txt")
    returns.monthly<-data.frame(matrix(ncol = 7, nrow = 0,
                     dimnames=list(NULL,c("COMPANY_ID","PRICEDM001","PRICEDM002","INSTALLDT","INSTALLNUM","PRICE_M001","PRICE_M002"))))    
    InstallNum<-1
    for (strDate in sipbInstallDates){
        print(paste("Starting:",strDate))
        price_filename<-paste(mainfolder,"/rdata/sip_prices_",strDate,".rdata",sep="")
        load(price_filename)  #price_data is variable name
        price_data<-price_data[,c("COMPANY_ID","PRICE_M002","PRICE_M001","PRICEDM002","PRICEDM001")]
#         zero_idx<-price_data[,"PRICE_M001"]==0 #these are bad, try to replace
#         zero_idx<-zero_idx*seq(1:nrow(price_data))
#         zero_idx<-zero_idx[zero_idx!=0]
#         if (length(zero_idx)>0) { #replace
#             for (i in zero_idx){
#                 k<-1
#                 found<-FALSE
#                 while (k<=6 & !found){  #search up to 6 months of 
#                     # look into the next price file for price 
#                     lookback<-GetPrice(price_data[i,"COMPANY_ID"],sipbInstallDates[InstallNum+k],k+1)
#                     if (! is.na(lookback)){
#                         if (price_data[i,"PRICE_M002"]==lookback[2] & price_data[i,"PRICE_M001"]!=0){ #to protect against splits
#                             price_data[i,"PRICE_M001"]<-lookback[1]
#                             found<-TRUE
#                         }    
#                     }
#                     k<-k+1
#                 }
#                 if (found){
#                     print(paste("Found    ",as.character(strDate),price_data[i,"COMPANY_ID"],sipbInstallDates[InstallNum+k],k))
#                 } else {
#                     #print(paste("Not Found",as.character(strDate),price_data[i,"COMPANY_ID"]))
#                 }    
#             }
#         }
        price_data<-price_data[complete.cases(price_data),]
        price_data$INSTALLDT <- strDate
        price_data$INSTALLNUM <- InstallNum
        returns.monthly<-rbind(returns.monthly,price_data)
        InstallNum<-InstallNum+1
    }
    sink()
    save(returns.monthly,file="returnsmonthly.rdata")
    beep(3)
    return(returns.monthly)
}

generate_returns<-function(){
    returns.monthly<-data.frame(matrix(ncol = 7, nrow = 0,
                                       dimnames=list(NULL,c("COMPANY_ID","PRICEDM001","PRICEDM002","RET","INSTALLDT","PRICE_M001","PRICE_M002"))))
    
    for (strDate in sipbInstallDates){
        print(as.character(strDate))
        sipfolder<-dbflocations(strDate)
        si_psdc <- loadfile("si_psdc.dbf", sipfolder$dbfs,F,c("COMPANY_ID","PRICE_M001","PRICE_M002"))
        si_psdd <- loadfile("si_psdd.dbf", sipfolder$dbfs,F,c("COMPANY_ID","PRICEDM001","PRICEDM002"))
        si_psdc<-tonumeric(si_psdc,c("PRICE_M001","PRICE_M002"))
        si_psdc$RET <- growthrate(si_psdc$PRICE_M002,si_psdc$PRICE_M001)
        z<-merge(si_psdd,si_psdc[,c("COMPANY_ID","RET","PRICE_M001","PRICE_M002")],by="COMPANY_ID")
        z<-z[complete.cases(z),]
        z$INSTALLDT <- strDate
        returns.monthly<-rbind(returns.monthly,z)
    }
    save(returns.monthly,file="returnsmonthly.rdata")
    beep(3)
    return(returns.monthly)
}
PreprocessPort<-function(holding.period=12){
    # identify bad returns
    out<-list()
    msgnum<-1
    ndates<-length(sipbInstallDates)
    for (i in 2:(ndates-holding.period)){  # for each portfolio
        port <-passing.lst[[i]]
        if (nrow(port)>0) {
            for (j in (i+1):(i+1+holding.period)){
                ret<-subset(returns.monthly,INSTALLDT==sipbInstallDates[j])
                y<-merge(port,ret,by="COMPANY_ID")
                z<-y$RET < -99.99 | is.na(y$RET) | y$RET >75
                if (sum(z)>0){
                    out[[msgnum]]<-y[z,c("COMPANY_ID","COMPANY","TICKER","RET","PRICEDM001","INSTALLDT")]
                    msgnum<-msgnum+1    
                }
            }    
        } else {
            out[[msgnum]]<-paste("Port",i,"Date",sipbInstallDates[i],"has no securities")
            msgnum<-msgnum+1
        }
    }
    return(out)
}

AllCompanyReturns<-function(CID){
    ret<-subset(returns.monthly,COMPANY_ID==CID)
}

portret<-function(PortNum,InstallDtIdx){
    port <- passing.lst[[PortNum]]
    port$WTS <- port.wts[PortNum]
    ret <- subset(returns.monthly,INSTALLDT==sipbInstallDates[InstallDtIdx])
    
}
