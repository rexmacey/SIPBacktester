smw<-function(strDate){
    library(foreign)
    source("sipbconfig.r")
    source("sipbutils.r")
    outflds<-c("COMPANY_ID","COMPANY","TICKER","MKTCAP","PRCHG_SD3Y","PRICE","PRICE_DATE")
    sipfolder<-dbflocations(strDate)
    
    si_ci <- loadfile("si_ci.dbf", sipfolder$static,F,c("COMPANY_ID","COMPANY","TICKER"))    
    si_mlt <- loadfile("si_mlt.dbf", sipfolder$dbfs,F,c("COMPANY_ID", "PBVPS"))
    si_isq <- loadfile("si_isq.dbf", sipfolder$static,F,c("COMPANY_ID", "EPSCON_Q1","EPSCON_Q2",
                                                          "EPSCON_Q5","EPSCON_Q6"))
    si_gr <- loadfile("si_gr.dbf", sipfolder$dbfs,F,c("COMPANY_ID", "EPSC_G5F","EPSC_G1Q5","EPSC_G2Q6"))
    si_perc <- loadfile("si_perc.dbf", sipfolder$dbfs,F,c("COMPANY_ID", "RRSW_4Q"))
    si_rat <- loadfile("si_rat.dbf", sipfolder$dbfs,F,c("COMPANY_ID", "PTM_12M"))
    si_psd <- loadfile("si_psd.dbf", sipfolder$dbfs,F,c("COMPANY_ID", "PRP_2YH","SHR_AQ1","MKTCAP","PRCHG_SD3Y","PRICE","PRICE_DATE"))
    
    data <- merge(si_ci,si_mlt,by="COMPANY_ID")
    data <- merge(data,si_isq,by="COMPANY_ID")
    data <- merge(data,si_gr,by="COMPANY_ID")
    data <- merge(data,si_perc,by="COMPANY_ID")
    data <- merge(data,si_rat,by="COMPANY_ID")
    data <- merge(data,si_psd,by="COMPANY_ID")
    varnames <- c("PBVPS","EPSCON_Q1","EPSCON_Q2","EPSCON_Q5","EPSCON_Q6", "EPSC_G5F", "RRSW_4Q",
                  "PTM_12M", "PRP_2YH", "SHR_AQ1","EPSC_G1Q5","EPSC_G2Q6","MKTCAP","PRCHG_SD3Y","PRICE")
    
    data <- tonumeric(data,varnames)
    
    
    out <- subset(data,PBVPS<=1.5 & EPSCON_Q1>EPSCON_Q5 & EPSCON_Q2>EPSCON_Q6 & EPSC_G1Q5 > EPSC_G2Q6 & 
                       EPSC_G5F>0 & RRSW_4Q>=70 & PTM_12M>0 & PRP_2YH>=85 & SHR_AQ1<=20)[,outflds]
    
    return(out)
}


