# all processing of scraped data of read from PDF files obtained from BBMP
library(httr)
library(wrapr)
library(stringdist)
library(urltools)
library(waffle)
library(ggrepel)
library(tidyverse)
library(lubridate)
library(htmlTable)
library(magrittr)
library(stringr)
library(pdftools)
library(splitstackshape)
library(tabulizer)
library(googledrive)
library(googlesheets4)
library(formattable)
library(readxl)
library(janitor)
library(data.table)
library(ggplot2)
library(jsonlite)
#load("imp.RData")
kridl_pattern="krid|technical mana|executive eng|rural infr|k[\\. ]*r[\\. ]*i[\\. ]*d[\\. ]*l"


# process the budget pdf file that has been read in using tabulizer's 'extract tables' function from bbmp site
# "http://bbmp.gov.in/documents/10180/18190157/3+years+budgetwise+summary.pdf/babe373f-70a9-471d-829f-d7f635ffe6cf"
pdfproc <- function(tab1){
        tab2 <- tab1 %>% map(~as.data.table(.x)) %>% rbindlist # covert the list of matrices to a DT
        tab2[,rn:=rownames(tab2) %>% as.numeric()] # need to add rownumber col
        suppressWarnings(tab2[,sn:=as.numeric(V1)])
        tab2[,sn2:=str_split(V1,"\\s",simplify = T)[1],by=rn]
        tab2[,V1:=ifelse(grepl("^\\d+\\s",V1), str_split(V1,"\\s",n = 2,simplify = T) %>% .[,2],V1)]
        suppressWarnings(tab2[,sn:=ifelse(is.na(sn),as.numeric(sn2),sn)][,sn2:=NULL])
        for(i in 1:6) tab2[,sn:=ifelse(is.na(sn),shift(sn),sn)] # cover upto 3 lines wrapped
        
        dt1 <- tab2[grepl("01-Apr",V2),.(rn,dstr=V2)] %>% rbind(tab2[grepl("01-Apr",V1),.(rn,dstr=V1)])
        dt1[,rn:=as.numeric(rn)]
        dt1 <- dt1[order(rn)]
        dt1[,gap:=-rn + shift(rn,type="lead")] # gap will match the exact row counts between transitions of dates
        dt1[,gap:=ifelse(is.na(gap),1,gap)]
        dt2 <- dt1[,.(gap=sum(gap)),dstr] %>% splitstackshape::expandRows(count="gap") # new trick before we cbind
        tab4 <- tab2[-c(1:2)] # first two lines are junk, and the date starts at row 3.
        stopifnot(nrow(tab4)==nrow(dt2))
        
        tab5 <- cbind(tab4,dt2)
        tab5[,fy:=str_remove(dstr," total") %>% str_sub(-2)]
        tab6 <- tab5 %>% unique(by=c("V1","V2","sn","fy"))
        tab6[,V1:=paste(V1,collapse = " ") %>% str_trim,by=.(sn,fy)]
        tab6[,V2:=paste(V2,collapse = " ") %>% str_trim,by=.(sn,fy)]
        tab6[,budhead:=ifelse(grepl("P\\d{4}",V1),V1,V2)] 
        tab7 <- unique(tab6,by=c("sn","budhead","fy"))
        
        tab7[,budcode:=ifelse(!is.na(sn),str_extract(budhead,"P\\d{4}"),NA)]
        tab7[,budhead:=ifelse(!is.na(sn),str_remove(budhead,"P\\d{4}") %>% str_trim,NA)]
        tab7[!is.na(budhead),.(fy,budcode,budhead,Count=as.numeric(V3),Estimate=as.numeric(V4))]
}

# WIP - start to write more code inside it. Remember there are three PDF files. Test on all 3.        
proc_prop_tax <- function(x=pt19,ptfile="2018-2019 PT Collections.pdf"){
  if(is.null(x)) x <- tabulizer::extract_tables(ptfile)
  tab2 <- x %>% map(~as.data.table(.x)) %>% rbindlist # covert the list of matrices to a DT
  tab2[,rn:=rownames(tab2) %>% as.numeric()]
  setnames(tab2,qc(zone,wardstr,nappl,amt))
}
        
# process the rtgs DT
proc_rtgs <- function(dt){
  dt %<>% map_at(c(1,2,6:8),as.numeric) %>% as.data.table
  dt[grepl("^Block",wodetails),status:="BLOCKED"]
  dt[grepl("^Block",wodetails),wodetails:=str_remove(wodetails,"Blocked")]
  dt[,jn:=str_extract(wodetails,"^\\d{3}-\\d{2}-\\d{6}")]
  dt[is.na(jn) & !grepl("^R",wodetails),jn:= str_extract(jn,".{10,20}-\\d{3}")]
  dt[is.na(jn) & grepl("^R",wodetails),status:= "R"]
  dt[is.na(jn) & grepl("^R",wodetails),jn:= str_extract(wodetails,"(?<=R-)\\d{3}-\\d{2}-\\d{6}")]
  dt[grepl("^R",wodetails),wodetails:=str_remove(wodetails,paste0("R-",jn))]
  dt[,contr_name:=str_extract(contractor,"^[^0-9]+(?=\\d)")]
  dt[,contr_no:=str_extract(contractor,"[0-9]{9,12}")]
  dt[,descr:=str_remove(wodetails,jn)]
  dt[,Dat_rtgs:=str_extract(brnumber,".{11}$") %>% dmy]
  dt[,Dat_cbr:=str_extract(brnumber,".{11}(?=Rtgs)") %>% dmy]
  dt[,Dat_br:=str_extract(brnumber,".{11}(?=CBR)") %>% dmy]
  dt[,brno:=str_extract(brnumber,"(?<=BR - ).{1,12}(?=.{14}CBR)")]
  dt[,cbrno:=str_extract(brnumber,"(?<=CBR - ).{6}")]
  dt[,rtgs:=str_extract(brnumber,"(?<=Rtgs - ).{6}")]
  setnames(dt,"id","wbid",skip_absent = T)
  dt
}

# process jobs raw DT  - wodetails is in next function, so this is reasonably fast
proc_jobs <- function(dt){
  #dt %<>% map_at(c(1,2,6:8),as.numeric) %>% as.data.table
  dt[,jn:=str_extract(job,"^\\d{3}-\\d{2}-\\d{6}")]
  # below line takes care of some garbage data in 16 records
  dt[is.na(jn),jn:=job %>% str_remove("-[MN]-W-.{2,6}-(AS|FO|IM)") %>% str_extract("^\\d{3}-\\d{2}-\\d{3}") %>% str_replace("(\\d{3})$",paste0("000","\\1"))]
  dt[,descr:=str_remove(job,jn)]
  dt[,fy:=str_sub(jn,5,6)]
  dt[,ward:=str_sub(jn,1,3) %>% as.numeric()]
  dt[,wbid:=as.numeric(wbid)][order(wbid)]
}

# output of above function may be piped here... this will take 10-15 minutes for 50K rows
proc_wodetails <- function(dt){
  dt <- split_wodetails(dt$wodetails) %>% cbind(dt,.) # this may take time
  dt[,job:=NULL][,wodetails:=NULL]
}

# split up wo details reliably into its 4 parts; pass a character array (or wo details raw string); return is a DT with 8 columns
split_wodetails <- function(wodetails){
  fn <- function(dates=dates,nos=nos){
    fn2 <- function(str,var) {
      ifelse(grepl(str,nos),var,NA) %>% na.omit %>% {ifelse(length(.)==0,NA_character_,as.character(.))}
    }
    wo_date= fn2("^WO",dates) %>% dmy
    sbr_date= fn2("^SBR",dates) %>% dmy
    br_date= fn2("^BR",dates) %>% dmy
    cbr_date= fn2("^CBR",dates) %>% dmy
    wono= fn2("^WO",nos)
    sbrno= fn2("^SBR",nos)
    brno= fn2("^BR",nos)
    cbrno= fn2("^CBR",nos)
    data.table(wdt=wo_date,sdt=sbr_date,bdt=br_date,cdt=cbr_date,
               wo=wono,sbr=sbrno,br=brno,cbr=cbrno)
  }
  ld <- wodetails %>% str_extract_all("\\d{2}-[JFMASOND][a-z]{2}-\\d{4}")
  ln <- wodetails %>% str_extract_all("WO-.{6}|SBR-.{6}|(?<=\\d)BR-.{6}|CBR-.{6}")
  map2(ld,ln,fn) %>% rbindlist()
}


# Returns a two column DT with job number and hand edited date. Also removes duplicated rows from intern sheet. Pass the column name that you want the hand edited DT for, so you get WO dates or Bill creation dates.
extract_handedits <- function(dt,var="hcwdt"){
  dt[,wbid:=as.numeric(wbid)]
  x1 <- dt[,.(wbid,jn)] %>% duplicated(fromLast=T) %>% which %>% {-1*.} %>% {dt[.,.(wbid,jn,get(var))][!is.na(V3)]}
  setnames(x1,"V3",var)
}

# merging the new filedt to the existing scr_bills_master (output of all wbid scrape_loop - sequential scrape)
merge_filedt <- function(old=scr_bills_master,new=filedt){
  if(!is.Date(new$dbrdate)) new <- proc_scr_columns(new)
  old %>% 
    full_join(new,by="wbid") %>% 
    mutate(nett=coalesce(nett.y,nett.x)) %>% 
    mutate(gross=coalesce(gross.y,gross.x)) %>% 
    mutate(deduction=coalesce(deduction.y,deduction.x)) %>% 
    mutate(sbrdate=coalesce(sbrdate.y,sbrdate.x)) %>% 
    mutate(dbrdate=coalesce(dbrdate.y,dbrdate.x)) %>% 
    mutate(cbrdate=coalesce(cbrdate.y,cbrdate.x)) %>% 
    mutate(sbrnumber=coalesce(sbrnumber.y,sbrnumber.x)) %>% 
    mutate(cbrnumber=coalesce(cbrnumber.y,cbrnumber.x)) %>% 
    mutate(dbrnumber=coalesce(dbrnumber.y,dbrnumber.x)) %>% 
    mutate(rtgsdate=coalesce(rtgsdate.y,rtgsdate.x)) %>% 
    mutate(billtype=coalesce(billtype.y,billtype.x)) %>% 
    mutate(currentlevel=coalesce(currentlevel.y,currentlevel.x)) %>% 
    mutate(currentlevelname=coalesce(currentlevelname.y,currentlevelname.x)) %>% 
    mutate(tvccrequiredyn=coalesce(tvccrequiredyn.y,tvccrequiredyn.x)) %>% 
    mutate(rtgs=coalesce(rtgs.y,rtgs.x)) %>% 
    mutate(contractorname=coalesce(contractorname.y,contractorname.x)) %>% 
    mutate(contractormobile1=coalesce(contractormobile1.y,contractormobile1.x)) %>% 
    mutate(contractoremail=coalesce(contractoremail.y,contractoremail.x)) %>% 
    mutate(rtgsdate=coalesce(rtgsdate.y,rtgsdate.x)) %>% 
    mutate(ddoname=coalesce(ddoname.y,ddoname.x)) %>% 
    mutate(category=coalesce(category.y,category.x)) %>% 
    mutate(wcdescription=coalesce(wcdescription.y,wcdescription.x)) %>% 
    mutate(pcode=coalesce(pcode.y,pcode.x)) %>% 
    mutate(budgethead=coalesce(budgethead.y,budgethead.x)) %>% 
    select(-contains(".x"),-contains(".y"),-budget)
}

# read the scraped files - takes care of duplicates
read_scraped_files <- function(pat="",fromdate="01-Nov-2020"){
  frdate= dmy(fromdate)
  files <- list.files(path = "scraped_bills",pattern = pat,full.names = T) %>% file.info() %>% as.data.table(keep.rownames = T)
  ffiles <- files[mtime>=frdate & size>0] # filtered files
  fnames <- ffiles$rn
  names(fnames) <- fnames
  dt <- 
    fnames %>%  
    imap(~fread(.x) %>% cbind(.y)) %>% 
    map_depth(.depth = 2,as.character) %>% # converting all to character to avoid error:  Internal error: column 3 of result is determined to be integer64 but maxType=='character' != REALSXP
    rbindlist(fill = T) 
  
  dt2 <- dt[ffiles[,.(rn,mtime)],on=.(.y=rn)]
  dt3 <- dt2[order(wbid,mtime)] %>% setkey(wbid)
  dt4 <- unique(dt3,fromLast = T,by = "wbid")
  dt4
}

# process new files (scraped by scrape_loop function) and rbind the output to scr_bills_master DT for unique wbids
proc_scr_columns <-  function(dt=NULL,kpat= kridl_pattern){
  dt[,kridl:=grepl(kpat,contractorname,ignore.case = T)]
  dt[,wbid:=as.numeric(wbid)]
  dt %<>% map_at(.at = c("deduction","gross","nett"),as.numeric) %>% as.data.table
  dt %<>% map_at(.at = c("cbrdate","dbrdate","sbrdate"),parse_date_time,order="ymd") %>% as.data.table
  dt %<>% map_at(.at = c("rtgsdate"), parse_date_time,order="dmy") %>% as.data.table
  dt %<>% map_if(.p = is.POSIXct, ~fifelse(.x<ymd(20000101),NA_Date_,as.Date(.x))) %>% as.data.table
  if("budget" %in% names(dt)){
    dt[,pcode:=str_extract(budget,"P\\d{4}")]
    if(dt[is.na(pcode),.N]>0) message("Detected NA Pcode in ",dt[is.na(pcode),.N]," rows.")
    dt[!is.na(pcode),budgethead:=str_remove(budget,pcode) %>% str_trim]
  }
  dt[,c("grossinwords","nettinwords","deductioninwords","budget"):=NULL]
  setDT(dt)
}

# a new column kridlis added when you pass scr_bills_master through this
add_kridl <- function(dt,kpat= kridl_pattern){
  dt[,kridl:=grepl(kpat,contractorname,ignore.case = T)]
}

# merges incremental scrapes with wbid non unique. 
# make sure both dt1, dt2 have the mtime column, as after order of mtime is used to remove duplicate: latest is retained.
merge_mult_wbids <- function(dt1,dt2=scr_bills_master_u){
  dt1[,wbid:=as.numeric(wbid)]
  dt2[,wbid:=as.numeric(wbid)]
  dt3 <- rbind(dt1,dt2,fill=T)
  dt4 <- dt3[order(wbid,mtime)] %>% setkey(wbid)
  dt5 <- unique(dt4,fromLast = T,by = "wbid")
  dt5
}

# range_read abnormal project -> zero columns A (a1) and W:X (a2) with col_types="DD
# output can be rbind with gszero (intern gsheet can be read from gszero.csv) to be fed as input to prep... functions
proc_abn_zero <- function(a1,a2,dt_jn_wbid=job_wbid){
  cbind(a1,a2) %>% setDT %>% setnames(c("jn","hcwdt","bcrdt")) %>% 
    filter(!is.na(hcwdt) | !is.na(bcrdt)) %>% left_join(dt_jn_wbid[,.(jn,wbid)])
}

# this will transform scraped data to the citizen portal data without keywords. 
# pass the raw scraped DTs as follows:
# scrape_billstatus() -> dt_billstatus_allwards
# scr_rtgs_dt <- scrape_payments()
prep_citzport <- function(kpat=kridl_pattern,
                          internDT=gszero,
                          jobdetails=scr_status_all2, # processed and wodetails split scraped status
                          scrbills=scr_bills_master,
                          rtgsdt=scr_rtgs_dt){
  new_rtgs_dt <- rtgsdt %>% proc_payments() %>% select(wbid,jn,everything()) %>% select(-contractor,-brnumber,-wodetails,-slno)
  bills_master <-  jobdetails[,.(jn,ward,fy,wbid,wo,wdt,descr)] %>% inner_join(scrbills) # ignoring description and other date fields that are already in the scraped bill detail files
  hand_wodt <- extract_handedits(internDT,var="hcwdt")
  hand_sbrdt <- extract_handedits(internDT,var="bcrdt")
  bills_master <- hand_sbrdt[hand_wodt[bills_master,on=.(jn,wbid)], on=.(jn,wbid)]
  setDT(bills_master)
  bills_master[,vwdt:=fifelse(is.na(hcwdt),wdt,hcwdt)]
  bills_master[,vsdt:=fifelse(is.na(bcrdt),sbrdate,bcrdt)]
  bills_master[,valmob:=clean_mobile(contractormobile1)]
  bills_master[,contr_mod:=ifelse(grepl(kpat,contractorname,ig=T),"M/S KRIDL Ltd.",contractorname) %>% str_remove(",\\s*$") %>% str_trim]
  moblookup  <- replace_wth_popular(bills_master[,.(name=contr_mod,value=valmob)])
  
  billsmast_newpaym <- 
    bills_master %>% left_join(new_rtgs_dt,by=c("wbid","jn")) %>% 
    mutate(nett= ifelse(is.na(nett.y),nett.x,nett.y),
           gross=ifelse(is.na(amount),gross,amount), # we are completely overwriting gross amount with the amount we get from the payments scrape that will happen periodically
           rtgsdate=fifelse(is.na(Dat_rtgs),rtgsdate,Dat_rtgs),  # same thing for RTGS date as the latest date will be available on the payment scrape
           rtgs=ifelse(is.na(rtgs.y),rtgs.x,rtgs.y)
           )
  
  payment_master <- billsmast_newpaym[nett>0,.(paidamt=sum(ifelse(!is.na(rtgsdate),nett,0)),
                                               totbillamt=sum(nett),
                                               prime_contr=.SD[,.(tot=sum(nett)),contr_mod][order(-tot)][1,contr_mod],
                                               Ncontr=uniqueN(contr_mod),
                                               avgpaydelay=mean(rtgsdate - vsdt,na.rm = T),
                                               Nbills=.N,Npaid=sum(ifelse(!is.na(rtgsdate),1,0)),
                                               descrpop=.SD[,.N,wcdescription][order(-N)][1,wcdescription  %<>% str_remove_all("\"") %<>% str_trim],
                                               fWODate=min(vwdt,na.rm = T),
                                               lBilldt=max(vsdt,na.rm = T),
                                               updated_on=max(mtime,na.rm = T),
                                               isComplete=grepl("Final",billtype) %>% any,
                                               budget = first(budgethead)
  ),by=.(jn,ward)
  ][,unpaidamt:=totbillamt - paidamt] %>% 
    left_join(moblookup,by=c("prime_contr"="name")) %>% 
    rename(mobile_prime_contr=pval)
  
  apprvd_projects    <-  payment_master %>% right_join(master_approval,by = c("jn","ward"))
  super_master <- rbind(apprvd_projects,payment_master[jn %in% setdiff(payment_master$jn,apprvd_projects$jn)],fill=T)
  setDT(super_master)
  super_master[,budgeted:=ifelse(is.na(description),F,T)]
  super_master[,description:=ifelse(is.na(description),descrpop,description)]
  super_master[,description:= str_trim(description %>% str_remove_all("\""))]
  super_master[is.na(budgethead) & !is.na(budget),budgethead:=budget]
  super_master[is.na(fy),fy:=str_sub(jn,5,6) %>% as.numeric()]
  super_master[,c("descrpop","budget","DateFrom","DateTo"):=NULL][order(jn)]
}

# Merge all scraped DTs with this one function.
prep_master_projectdt <- function(kpat=kridl_pattern,
                       zerodt = fread("gszero.csv"), # intern dataset for WO correction
                       mstr_appr = master_approval, # containing all approved jobs
                       jwmap = job_wbid_map, # mainly for wbid - jn mapping
                       scrbills =filedt2, # sequential scrape DT - filedt
                       rtgsdt = scr_rtgs_dec15){ # fresh rtgs data 
  jw <- jwmap %>% map_if(is.Date,as.Date) %>% as.data.table() # to take care of IDate formats when reading a csv
  
  bills <- 
    scrbills %>% mutate(clean_mob=clean_mobile(contractormobile1)) %>% 
    replace_with_popular(key = "contractorname",val="clean_mob") %>% 
    right_join(scrbills,by = "contractorname") %>% 
    mutate(contractormobile1=coalesce(clean_mob,contractormobile1))
  
  # rtgsdt %>% 
  #   mutate(clean_mob=clean_mobile(contr_no)) %>% 
  #   replace_with_popular(key = "contr_name",val="clean_mob")
  
  dt_main <- 
    jw %>% 
    full_join(bills,by="wbid") %>% 
    mutate(description=coalesce(descr,wcdescription),sbrdate=coalesce(sdt,sbrdate), cbrdate=coalesce(cdt,cbrdate)) %>% 
    full_join(rtgsdt,by=c("wbid","jn")) %>% 
    mutate(nett=coalesce(nett.y,nett.x),
           rtgsdate=coalesce(Dat_rtgs,rtgsdate),
           contractorname=coalesce(contr_name,contractorname),
           contractormobile1=coalesce(contr_no,contractormobile1),
           nett=coalesce(amount,nett),
           cbrdate=coalesce(cbrdate,Dat_cbr)) %>% 
    full_join(mstr_appr,by="jn") %>% 
    mutate(budgethead=coalesce(budgethead.x,budgethead.y),pcode=coalesce(pcode.x,pcode.y),
           description=coalesce(description.x,description.y)) %>% 
    select(-contains(".y"), -contains(".x"), -matches("DateF|DateT|kridl")) %>% 
    mutate(ward=str_sub(jn,1,3) %>% as.numeric,kridl=ifelse(is.na(nett),NA,grepl(kpat,contractorname,ig=T)))
  setDT(dt_main)
  x1 <- 
    dt_main[is.na(jn)] %>% 
    select(-jn) %>% 
    left_join(mstr_appr,by="description") %>% 
    select(-contains(".y"), -contains(".x"), -matches("DateF|DateT|kridl")) %>% 
             filter(!is.na(jn))
  setDT(x1)
  master_negative <- 
    dt_main[!(jn %in% x1$jn | wbid %in% x1$wbid) ]
  
  master_dt2 <- bind_rows(master_negative,x1)
  setDT(master_dt2)
  master_dt2[,contr_mod:=ifelse(grepl(kridl_pattern,contractorname,ig=T),"M/S KRIDL Ltd.",contractorname) %>% str_remove(",\\s*$") %>% str_trim]
  master_dt2
}

#clean up WO dates and SBR dates
clean_dates <- function(dt,internDT=gszero){
  hand_wodt <- extract_handedits(internDT,var="hcwdt")
  hand_sbrdt <- extract_handedits(internDT,var="bcrdt")
  dt %<>% left_join(hand_wodt,by=c("jn","wbid")) %>% mutate(wodate=coalesce(hcwdt,wdt))
  dt %<>% left_join(hand_sbrdt,by=c("jn","wbid")) %>% mutate(sbrdate=coalesce(bcrdt,sbrdate))
  setDT(dt)
}

# pass a character vector of mobile numbers. All invalid numbers wll be replaced by NA.
clean_mobile <- function(x){
  x %>% str_remove("^(990{8}|9{9}|8{9}|7{9})") %>% str_extract("[7-9][0-9]{9}")
}

# used to select the most used mobile for a specific contractor; pass the key and value variables, returns a unique DT
replace_with_popular <- function(dt,key="contractorname",val="contractormobile1"){
  dt2 <- dt[,.SD,.SDcols=c(key,val)]
  dt2[,{t <- .SD[,.N,val][order(-N)][1:2,c(1)];coalesce(t[1],t[2])}, by=key] # possibly the shortest code to get popular values
}

# pass the DT of 6 columns downloaded by range_read(..,sheet="inspection",range=A:J",col_types="ccc__c__cc")
clust_contr_names <- function(dt){
  dt <- setDT(dt[,c(1:6)])
  setnames(dt,qc(mob,contr_name_clean,oclust,nclust,fpcode,who))
  #dt[,clust:=coalesce(nclust,oclust)]
  dt[,fpcode:=ifelse(grepl("^[a-z]$",fpcode,ig=T),paste0(nclust,fpcode),fpcode)]
  dt[,fclust:=ifelse(is.na(fpcode),nclust,ifelse(fpcode==1,paste0("A",seq_len(.N)),fpcode))]
}
# summarise all bills at job level.
conv_to_jobwise <- function(dt){
  payment_master <- dt[nett>0,.(paidamt=sum(ifelse(!is.na(rtgsdate),nett,0)),
                                               totbillamt=sum(nett),
                                               prime_contr=.SD[,.(tot=sum(nett)),contr_mod][order(-tot)][1,contr_mod],
                                               Ncontr=uniqueN(contr_mod),
                                               avgpaydelay=mean(rtgsdate - vsdt,na.rm = T),
                                               Nbills=.N,Npaid=sum(ifelse(!is.na(rtgsdate),1,0)),
                                               descrpop=.SD[,.N,wcdescription][order(-N)][1,wcdescription  %<>% str_remove_all("\"") %<>% str_trim],
                                               fWODate=min(vwdt,na.rm = T),
                                               lBilldt=max(vsdt,na.rm = T),
                                               updated_on=max(mtime,na.rm = T),
                                               isComplete=grepl("Final",billtype) %>% any,
                                               budget = first(budgethead)
  ),by=.(jn,ward)
  ][,unpaidamt:=totbillamt - paidamt] %>% 
    left_join(moblookup,by=c("prime_contr"="name")) %>% 
    rename(mobile_prime_contr=pval)
  
}

# cool function : saw after a month and still cool
add_keywords <- function(dt){
  var<- grep(pattern = "description",names(dt),ig=T,val=T)[1]
  varbud<- grep(pattern = "bud",names(dt),ig=T,val=T)[1]
  lkeys <- 
    list(
      dt[,fifelse(get(var) %>% str_detect(regex("dr[ai]{2}n|SWD|storm|desilt|rajakalve|missing.{1,20}slab",ig=T)),"Drains",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("rcc.{1,6}drain",ig=T)),"Drains",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("(wide?nin|improve|cc|construction|repairs|deve?lop).{1,35}road|road.?cut|roads|carpeting|flyover|asphalt|compreh[a-z]* dev[a-z]",ig=T)),"Roads",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("white.?topping|resurfac|conserv[ae]ncy|new road",ig=T)),"Roads",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("(laying.{1,10}concret)|concret.+(road|cross)",ig=T)),"Roads",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("road restor|emulsion|pot.?hole",ig=T)),"Roads",NA_character_)],
      dt[,fifelse(get(varbud) %>% str_detect(regex("roads? dev|White Topping",ig=T)),"Roads",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("footpath|foot path|pedestrian|kerb|path.?way|pavement",ig=T)),"Footpaths",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("culv[ae]rt",ig=T)),"Culverts",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("led\\b|str.{1,8}light|lighting|lights|light fit|sodium|vapour",ig=T)),"Lightings",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("bore.?well|borwell|bor.?well|submersible|(water|rcc).?pipe.?line|water.?supply|pump|motor|water purifi",ig=T)),"Drinking water",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("R\\.?\\s?O\\.?\\s?(plant|treat|water)",ig=T)),"Drinking water",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("drinking water|tanker|rain water|water.*storage",ig=T)),"Drinking water",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("recharging pit",ig=T)),"Drinking water",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("emer?ge|Emrjency",ig=T)),"Emergency expense",NA_character_)],
      dt[,fifelse(get(varbud) %>% str_detect(regex("emergency",ig=T)),"Emergency expense",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("toilet",ig=T)),"Toilets",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("(name|orn|sign|ward|repaint).{1,5}board|sticker",ig=T)),"Name Boards",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("improvement.{1,15}park",ig=T)),"Parks",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("parks|(children|play|gym).{1,5}equipment|play.?ground|railing.*park",ig=T)),"Parks",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("(impro?ve?ment|m[aei]{1,2}nt[aei]{0,2}n[aei]{1,2}nce|deve?lop).{1,40}park",ig=T)),"Parks",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("development.{1,5}garden",ig=T)),"Parks",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("(proposed|circular).{1,2}park",ig=T)),"Parks",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("(\\bat\\b|\\bto\\b).{4,25}park",ig=T)),"Parks",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("open gym|fencing.+park",ig=T)),"Parks",NA_character_)],
      dt[,fifelse(get(varbud) %>% str_detect(regex("(?<!other than .{1,20})parks",ig=T)),"Parks",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("imm?err?ss?ion|idol\\b|lord|festival|dhyan.+mandira.+temple|prayer hall|prar?thn.{1,5}mandir|hindu",ig=T)),"Religious",NA_character_)],
      dt[,fifelse(get(varbud) %>% str_detect(regex("imm?err?ss?ion|idol",ig=T)),"Religious",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("(ref?er?al|general|matern|govern|improvement|repair|(Multi|Super) speciality).{1,32}hospital",ig=T)),"Health",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("hospital build|dialysis",ig=T)),"Health",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("at.{1,15}hospital|primary health cen",ig=T)),"Health",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("cc.*tv|camera",ig=T)),"CCTV",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("(?<!(desil).{1,50})debri(?!.*desil)",ig=T,perl=T)),"Solid Waste Mgmt",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("(?<!(drain|swd).{1,50})silt.+tractor",ig=T,perl=T)),"Solid Waste Mgmt",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("removal.+silt|leaf.?pit|over.?bu.+earth",ig=T)),"Solid Waste Mgmt",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("SWM|solid.?waste|compost",ig=T,perl=T)),"Solid Waste Mgmt",NA_character_)],
      dt[,fifelse(get(varbud) %>% str_detect(regex("SWM|Solid Waste",ig=T,perl=T)),"Solid Waste Mgmt",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("garbage|solid waste|dustbin",ig=T,perl=T)),"Solid Waste Mgmt",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("construction.{1,15}(indoor|building|complex|school|stadium|bha[wv]an|hospital|cente?r|badmin|yoga|dhyan.?mandir|rang.?mandir)",ig=T)),"New Building",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("statue|indoor gym|auditorium|senior cit.+rest room",ig=T)),"New Building",NA_character_)],
      dt[,fifelse(get(varbud) %>% str_detect(regex("construction of (houses|meeting|meditation)",ig=T)),"New Building",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("(providing|constr|repair).{1,20}(school|college)",ig=T)),"Schools",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("angana?[vw]ad",ig=T)),"Schools",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("school build|class room",ig=T)),"Schools",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("lake",ig=T)),"Lakes",NA_character_)],
      dt[,fifelse(get(varbud) %>% str_detect(regex("lake",ig=T)),"Lakes",NA_character_)]
    )
  keyword_array <- lkeys %>%  pmap(c) %>% map(unique) %>% map(na.omit) %>% map_chr(paste,collapse=", ") # the killer code
  dt[,keywords:=keyword_array] 
  dt[nchar(keywords)<2,keywords:=NA_character_] # remove blanks
  dt[grepl("Drains.+Health",keywords),keywords:= keywords %>% str_remove_all(", New Building|, Health")]
  dt[grepl("Roads.+Health",keywords),keywords:= keywords %>%  str_remove_all(", New Building|, Health")]
  dt[grepl("Roads.+School",keywords),keywords := keywords %>% str_remove_all(", New Building|, Schools")]
  dt[grepl("Drains.+School",keywords),keywords:= keywords %>% str_remove_all(", New Building|, Schools")]
}

# pass the DT output from prep citizen portal function above. The column names are pre transformation
keyword_split <- function(dt=citzport_test,w=150,yearly=F,expense=F){
  if(yearly==T) bstr=c("keywords","fy") else bstr <- c("keywords")
  if(expense==T) varamt <- "totbillamt" else varamt <- "approvedamount"
  
  if(w=="ALL"){
    bstr2 <- c(bstr,"ward")
    wtall <- dt %>% cSplit("keywords",sep=",",direction = "tall")
    output <- wtall[,{
      kamt <- .SD[,.(totamt=sum(get(varamt),na.rm = T)),by=bstr2]
      #totamt <- dt[,.(totamt=sum(get(varamt),na.rm = T)),by=bstr2]
      kamt[,prc:=percent(totamt/sum(totamt,na.rm = T)),by=ward]
    }] 
  } else {
  dt[ward==w] -> wlevdt
  wlevdt %>% add_keywords()
  wtall <- wlevdt %>% cSplit("keywords",sep=",",direction = "tall")
  output <- wtall[,{
    kamt <- .SD[,.(totamt=sum(get(varamt),na.rm = T)/1e7),by=bstr]
    #totamt <- wlevdt[,sum(get(varamt),na.rm = T)]
    kamt[,prc:=percent(totamt/sum(totamt,na.rm = T))]
    #kamt[,estamt:=totamt*prc/1e7]
    kamt
  }] 
  }
  output 
}


ews_rep <- function(dt=citzport_test){
  frivolous_words <- c("\\bof","\\bat","\\bon","\\bin","\\bto","\\bfor","\\bit","\\bits","\\bfrom","\\bbehind","\\band")
  dt2 <- dt[grepl("constr.+individ.{1,10}h[ou]{1,2}se.+in ward.+",description,ig=T)]
  dt3 <- dt[grepl("constr.+individ.{1,10}h[ou]{1,2}se",description,ig=T)] %>% fsetdiff(dt2)
  dt3[,truncdesc:="CONSTRUCTION OF INDIVIDUAL HOUSES"]
  
  dt3a <- dt3[,.(appr=sum(approvedamount),spent=sum(totbillamt,na.rm = T),.N), by=.(descr=truncdesc)]
  
  dt2[,
      truncdesc:=description %>% 
        tolower %>% 
        str_extract(".+(?=in ward)") %>% 
        str_remove_all(pattern = paste(frivolous_words,collapse="\\b|")) %>% 
        str_remove_all("\\d") %>% 
        str_remove_all("[:punct:]") %>% 
        str_squish
      ]
  
  dt2[,truncdesc:=ifelse(grepl("CHUNA LANE",description,ig=T),"CONSTRUCTION OF INDIVIDUAL HOUSES",truncdesc)]
  
  dt2a <- dt2[,.(appr=sum(approvedamount),spent=sum(totbillamt,na.rm = T),.N),
     by=.(descr=
            truncdesc %>% 
            str_remove_all("\\d") %>% 
            tolower %>% 
            str_trim %>% 
            str_replace_all("ben[ei]{1,2}f[ie]c[a-z]*","beneficiary") %>% 
            str_squish %>% 
            
            str_replace(regex(".*sc.?st.*|schedule.caste.+schedule.tribe",ig=T),"Construction of individual houses for SC/ST beneficiary ") %>% 
            str_replace(regex(".*bcm.*",ig=T),"Construction of individual houses for BCM beneficiary") %>% 
            str_replace(regex(".*obc.*",ig=T),"Construction of individual houses for OBC beneficiary") %>% 
            str_replace(regex(".*sc.?cat.*|.*sc.?ben.*",ig=T),"Construction of individual houses for SC category") %>% 
            str_replace(regex(".*st.?cat.*|.*st.?ben.*",ig=T),"Construction of individual houses for ST category")
          )]
  rbind(dt2a,dt3a)
}

# modified previous keyword function to tag EWS schemes.
add_ews_tags <- function(dt){
  var<- grep(pattern = "description",names(dt),ig=T,val=T)[1]
  varbud<- grep(pattern = "bud",names(dt),ig=T,val=T)[1]
  lkeys <- 
    list(
      dt[,fifelse(get(var) %>% str_detect(regex("constr.+individ.{1,10}h[ou]{1,2}se",ig=T)),"INDIV-HOUSE",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("sc.?st|schedule.caste.+schedule.tribe",ig=T)),"SC/ST",NA_character_)],
      dt[,fifelse(get(varbud) %>% str_detect(regex("BCM")),"BCM",NA_character_)],
      dt[,fifelse(get(varbud) %>% str_detect(regex("\\bSC")),"SC",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("\\bSC\\b|\\bST\\b")),"SC/ST",NA_character_)],
      dt[,fifelse(get(varbud) %>% str_detect(regex("ews",ig=T)),"EWS",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("\\bews",ig=T)),"EWS",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("backward|minorities",ig=T)),"BACKWARD",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("sc.?cat|sc.?ben",ig=T)),"SC",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("obc",ig=T)),"OBC",NA_character_)],
      dt[,fifelse(get(var) %>% str_detect(regex("st.?cat|st.?ben",ig=T)),"ST",NA_character_)]
    )
  keyword_array <- lkeys %>%  pmap(c) %>% map(unique) %>% map(na.omit) %>% map_chr(paste,collapse=", ") # the killer line of code
  dt[,ews_keywords:=keyword_array] 
  dt[nchar(ews_keywords)==0,ews_keywords:=NA_character_] # remove blanks
  dt[grepl("SC/ST.+SC",ews_keywords),ews_keywords:= ews_keywords %>% str_remove_all("SC$| SC,") %>% str_remove(",\\s*$")]
  dt[grepl("SC/ST.+ST",ews_keywords),ews_keywords:= ews_keywords %>% str_remove_all("ST$| ST,")  %>% str_remove(",\\s*$")]
}


# Pass the tabulizer list of matrices as output by using following table extract command:
# tabulizer::extract_tables("BBMPR_ward_master_BBMP Restructuring 03-08-2015.pdf")
# load the table on googlesheet and clean after 192 ward.
proc_ward_pdf <- function(lmat){
  ppage <- function(mat){
    dt <- as.data.table(mat)
  }
  lmat %>% map_dfr(ppage) %>% 
    row_to_names(1) %>% 
    clean_names() %>% 
    map_at(vars(matches(c("ward_no","pop","area","numb","kms","fire","rate"),ignore.case = T)),~ str_remove_all(.x,"[%,]") %>% as.numeric ) %>% 
    as.data.table() %>% 
    .[,rn:=seq_along(ward_no)]
}


# ====== PROCESS RECEIPTS ==========

# fire the correct RDS pattern to read from the path
proc_rcpts <- function(pat=".RDS",dpath="~/Dropbox/bbmp"){
  masterdt <- data.table()
  list.files(dpath,pattern = pat,full.names = T) -> all_files
  for(i in all_files){
    filesn <- str_extract(i,"(?<=fy...)...")
    data <- readRDS(i)
    if(ncol(data) !=10) {
      message("Warning: File ",i, " has ",ncol(data)," columns instead of standard 10 cols. Saving data till now")
      saveRDS(masterdt,"masterdt.RDS")
    }
    if(nrow(data)==0)  message("Warning: File ",i, " has ",0," rows !")
    setnames(data,old="id.id",new="id",skip_absent=T)
    masterdt <- rbind(masterdt,data,fill=T)
    cat(filesn,":",nrow(data),"; ",sep = "")
  }
  masterdt
}


# ======= PROCESS CITIZEN PORTAL FEEDBACK ======
proc_crowd_ratings <- function(downloaded_file="Project-Ratings.csv"){
 fread(downloaded_file) %>% select(-"Actions") %>% setnames(c("jn","name","email","mobile","r1","r2","comments","dttime")) %>% mutate(ward=str_sub(jn,1,3) %>% as.numeric(),mobile=as.character(mobile),dttime=mdy_hm(dttime,tz="Asia/Kolkata")) %>% filter(dttime  >= ymd(20201215))
}



#=== DCBILLS PROCESSING======

# pass the raw scraped DT to this function to transform it
proc_dcbills <- function(dt){
  if( "slno" %in% names(dt)) dt[,slno:=NULL]
  dt[,gross:=as.numeric(amount)]
  dt[,date_rtype:=rtype %>% str_extract("\\d{2}-[A-Z]..-\\d{4}") %>% dmy %>% as.IDate()]
  dt[,date_rtgs:=rtgsdetails %>% str_extract("\\d{2}-[A-Z]..-\\d{4}") %>% dmy %>% as.IDate()]
  dt[,rtgs_no:=rtgsdetails %>% str_extract("(?<=/ )\\d{6}") %>% as.numeric()]
  dt[,rtgs_code:=rtgsdetails %>% str_extract("[a-z]+")]
  dt[,ifms:=rtype %>% str_extract("ifms\\d+")]
  dt[,rtype_sn:=rtype %>% str_extract("\\d{6}$") %>% as.numeric()]
  dt[,pcode:=str_extract(budgethead,"P\\d{4}")]
  dt[,ddo:=str_sub(ddoname,4,6) %>% as.numeric]
  dt[,zone:=str_sub(rtgs_code,4,6)] %>% unique
  
}

