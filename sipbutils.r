# Utility functions for SIPBacktester

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

x<-passing.lst[[1]]
y<-port_wts_eq(x)
