# Utility functions for SIPBacktester
library(foreign)

loadfile<-function(fn,folder,prnt=T,flds=NULL){
    # loads the fields (all fields if flds==NULL) from dbf fn in folder
    if (is.null(flds)){
        data<-read.dbf(file=paste(folder,"/",fn,sep=""),as.is = T)    
    } else {
        data<-read.dbf(file=paste(folder,"/",fn,sep=""),as.is = T)[,flds]
    }
    if(prnt){
        print(str(data))
    }
    return(data)
}

tonumeric<-function(data,varnames){
    for (v in varnames){
        data[,v] <- as.numeric(data[,v])
    }
    return(data)
}

growthrate<-function(val.from, val.to){
    pos.idx <- val.to >= val.from & val.from!=0 & !is.na(val.from) & !is.na(val.to)
    neg.idx <- val.to < val.from & val.from!=0 & !is.na(val.from) & !is.na(val.to)
    na.idx <- is.na(val.from) | is.na(val.to) | val.from==0
    out <- val.from*0
    out[pos.idx] <- abs(100*(val.to[pos.idx]-val.from[pos.idx])/val.from[pos.idx])
    out[neg.idx] <- -abs(100*(val.to[neg.idx]-val.from[neg.idx])/val.from[neg.idx])
    out[na.idx]<-NA
    return(out)
}

port_wts<-function(stock.df,method="eq",maxwt=1){
    # returns a set of wts for each stock
    # methods include 'eq' for equal wtd, and 'cap' for market cap weighted
    # maxwt is maximum wt allowed
    # output is a list with two items wts for stock wts and cash for weighting to cash
    out=list()
    if (nrow(stock.df)==0) {
        out$wts <- NULL
        out$cash <- 1.0
    } else {
        out<-switch(method,
                    eq=port_wts_eq(stock.df,maxwt),
                    cap=port_wts_cap(stock.df,maxwt))
    }
    return(out)
}

port_wts_eq<-function(stock.df,maxwt=1){
    out<-list()
    out$wts <- rep(min(maxwt,1/nrow(stock.df)),nrow(stock.df)) # equal wt including max
    out$cash <- 1 - sum(out$wts) # this will be > 0 if max is actually a constraint
    return(out)
}

port_wts_cap<-function(stock.df,maxwt=1){
    out<-list()
    totalmktcap <- sum(stock.df[,"MKTCAP"])
    out$wts <- apply(cbind(rep(maxwt,nrow(stock.df)), stock.df[,"MKTCAP"] / totalmktcap),1,min)  #cap wtd with maximum
    sumwts<-sum(out$wts)  # will be 1 if maxwt isn't a constraint, otherwise <1
    unallocated <- 1-sumwts
    capacity <- maxwt - out$wts[out$wts<maxwt]
    if (unallocated > sum(capacity)){ #even if we maxwt all stocks, there's still some left over
        out$wts[out$wts<maxwt]<-rep(maxwt,sum(out$wts<maxwt)) # assign max wt to all stocks
        out$cash <- max(0,unallocated - sum(capacity))  #assign unallocated to cash
    } else { #capacity is greater than unallocated, so no cash
        out$cash <- 0
        while (unallocated > 0){
            out$wts[out$wts<maxwt]<-out$wts[out$wts<maxwt] + min(maxwt - out$wts[out$wts<maxwt],
                                                                 unallocated*out$wts[out$wts<maxwt]/sum(out$wts[out$wts<maxwt]))
            unallocated<-1-sum(out$wts)
        }
    }
    return(out)
}

convert_price_files_to_rdata<-function(strDate){ #convert a single pair of price and date files to rdata
    library(reshape)
    sipfolder<-dbflocations(strDate)
    si_psdc <- loadfile("si_psdc.dbf", sipfolder$dbfs,F)
    si_psdd <- loadfile("si_psdd.dbf", sipfolder$dbfs,F)
    #var<-paste("pr_",strDate,sep="")
    #assign(var,merge(si_psdc,si_psdd,by="COMPANY_ID"))
    price_data<-merge(si_psdc,si_psdd,by="COMPANY_ID")
    price_vars<-sprintf("PRICE_M%03d",seq(1:120))
    price_data <- tonumeric(price_data,price_vars)
    outfile<-paste(mainfolder,"/rdata/sip_prices_",strDate,".rdata",sep="")
    save(price_data,file=outfile)
}

convert_all_price_files_to_r<-function(){  #convert all files
    lapply(sipbInstallDates,convert_price_files_to_rdata)
}

convert_1install_dbf_to_rdata<-function(strDate){
    sipfolder<-dbflocations(strDate)
    si_ci <- loadfile("si_ci.dbf", sipfolder$static,F)
    si_mlt <- cleanfile(loadfile("si_mlt.dbf", sipfolder$dbfs,F))
    si_isq <- cleanfile(loadfile("si_isq.dbf", sipfolder$static,F))
    si_gr <- cleanfile(loadfile("si_gr.dbf", sipfolder$dbfs,F))
    si_perc <- cleanfile(loadfile("si_perc.dbf", sipfolder$dbfs,F))
    si_rat <- cleanfile(loadfile("si_rat.dbf", sipfolder$dbfs,F))
    si_psd <- cleanfile(loadfile("si_psd.dbf", sipfolder$dbfs,F))
    save(si_ci,file=paste(mainfolder,"/rdata/si_ci_",strDate,".rdata",sep=""))
    save(si_mlt,file=paste(mainfolder,"/rdata/si_mlt_",strDate,".rdata",sep=""))
    save(si_isq,file=paste(mainfolder,"/rdata/si_isq_",strDate,".rdata",sep=""))
    save(si_gr,file=paste(mainfolder,"/rdata/si_gr_",strDate,".rdata",sep=""))
    save(si_perc,file=paste(mainfolder,"/rdata/si_perc_",strDate,".rdata",sep=""))
    save(si_rat,file=paste(mainfolder,"/rdata/si_rat_",strDate,".rdata",sep=""))
    save(si_psd,file=paste(mainfolder,"/rdata/si_psd_",strDate,".rdata",sep=""))
}


cleanfile<-function(df){
    XNullCol<-match("X_NullFlags",colnames(df))
    if (! is.na(XNullCol)){
        df<-df[,1:(XNullCol-1)]
    }
    for (i in 2:ncol(df)){
        df[,i]<-as.numeric(df[,i])
    }
    return(df)
}
temp<-cleanfile(si_mlt)