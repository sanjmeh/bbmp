# scrape bbmp financial records
library(tidyverse)
library(tidyselect)
library(lubridate)
library(htmlTable)
library(wrapr)
library(data.table)
library(magrittr)
library(stringr)
library(pdftools)
library(splitstackshape)
library(tabulizer)
library(googlesheets4)
library(formattable)
library(readxl)
library(janitor)
library(summarytools)
library(stringdist)
library(ggplot2)
library(googlesheets4)
library(googledrive)
library(tictoc)
library(jsonlite)
library(httr)
library(urltools)
# urlb <- readRDS("url_bbmp.RDS")
# urla <- readRDS("url_bbmp_approvals.RDS")
# load("urls3_approved_projects.RData")
load("urls.RData")
kpat <- "krid|technical mana|executive eng|Rural Infr|k\\.r\\.i\\.d\\.l"
jscrape <- function(x) read_lines(x) %>% fromJSON %>% as.data.table 



# old scrape function - not used...works on  jobnumber_url & master_payments_url
scrape_bbmp <- function(link){
        y1 <- read_html(link)
        y2 <- rvest::html_nodes(y1,css = "p") %>% html_text() %>% jsonlite::fromJSON() %>% as.data.table
        
}

# sequential scraping for  bill status. Pass wbid text array as input in ids.  Fetches records per bill id ('wbid'). No Job Number included in output
# first find out the missing wbids needed. This scraping is slow @ 500 ~ 1000 bills per hour or so.
scrape_loop <- function(ids,baseurl="http://218.248.45.166:8092/vssWB/vss00CvStatusData.php?pAction=LoadDetails&pWorkBillID="){
        idlist <- split(ids,as.numeric(str_sub(ids,-2)))
        final_dt <- idlist %>% map(loop1) %>% rbindlist(idcol = "grp")
        fwrite(final_dt,file = "finalfile.csv",dateTimeAs = "write.csv")
}

fy <- function(Date) ifelse( between(month(Date),4,12), year(Date) + 1, year(Date))

# called by scrape_loop; files are written in chunks of 100 or 10 or 1000 (depends on the str_sub parameter)
loop1 <- function(billid){
        links <- paste0("http://218.248.45.166:8092/vssWB/vss00CvStatusData.php?pAction=LoadDetails&pWorkBillID=",billid)
        dt <- data.table()
        for (i in links) {
                oldrows <- nrow(dt)
                #Sys.sleep(as.numeric(billid)%%10)
                dt_new <-  jsonlite::fromJSON(i) %>% as.data.table()
                dt_new[,wbid:=str_extract(i,"\\d*$")]
                if (length(dt_new)<10){
                        message("\nMissing bill_id:",i)
                        readr::write_lines(i,"missing_bill_ids.txt",append = T)
                        next
                }
                dt <- rbind(dt,dt_new,fill=T)
                cat("\nAdded: ", nrow(dt) - oldrows," rows from url: ",i)
        }
        fwrite(dt,file = paste0("scraped_bills/","nov4_",first(billid),"_",last(billid)),dateTimeAs = "write.csv")
        return(dt)
}

# get billing code
scrape_code <- function(url,code) {
        dt <- constr_tree_url(url = urlb,id=code)
        if(length(dt)<3) message("Nothing found for code: ",code)
        return(dt)
}

# process new files (scraped by scrape_loop function) and rbind the output to scr_bills_master DT for unique wbids
proc_scr_files <-  function(dt=NULL,pat="^nov",kpat= "krid|technical mana|executive eng|Rural Infr|k\\.r\\.i\\.d\\.l"){
        if(is.null(dt)) dt <- 
                        list.files(path = "scraped_bills/",pattern = pat,full.names = T) %>% 
                        map(fread) %>% map_depth(.depth = 2,as.character) %>% 
                        rbindlist
        dt[,kridl:=grepl(kpat,contractorname)]
        dt %<>% map_at(.at = c("deduction","gross","nett"),as.numeric) %>% as.data.table
         dt %<>% map_at(.at = c("cbrdate","dbrdate","sbrdate"),parse_date_time,order="ymd") %>% as.data.table
         dt %<>% map_at(.at = c("rtgsdate"), parse_date_time,order="dmy") %>% as.data.table
         dt %<>% map_if(.p = is.POSIXct, ~fifelse(.x<ymd(20000101),NA_Date_,as.Date(.x))) %>% as.data.table
         # dt[,pcode:=str_extract(budget,"P\\d{4}")]
         # dt[,budhead:=str_remove(budget,pcode) %>% str_trim]
        dt[,c("grossinwords","nettinwords","deductioninwords"):=NULL]
}

# new url scraping... WIP
loop_url <- function(url){
        dt <- data.table()
        for (i in url) {
                oldrows <- nrow(dt)
                dt_new <-  read_lines(i) %>%  fromJSON %>% as.data.table()
                #dt_new[,wbid:=str_extract(i,"\\d*$")]
                if (length(dt_new)<3){
                        message("\nMissing data for:",param_get(i,"pDateTo")[[1]])
                        next
                } else 
                        dt_new[,DateTo:=param_get(i,"pDateTo") %>% dmy]
                dt_new[,DateFrom:=param_get(i,"pDateFrom") %>% dmy]
                
                dt <- rbind(dt,dt_new,fill=T)
                cat("..Added: ", nrow(dt) - oldrows," rows for pID:", param_get(i,"pID")[[1]],"between",param_get(i,"pDateFrom")[[1]]," and ",param_get(i,"pDateTo")[[1]],"\n")
        }
        #fwrite(dt,file = paste0("scraped_approval_amounts/","trial_",first(billid),"_",last(billid)),dateTimeAs = "write.csv")
        return(dt)
}

compare_urls <- function(x,y,type=2){
        dt1 <- param_get(x) %>% as.data.table
        dt2 <- param_get(y) %>% as.data.table
        if(type==1) print(compare_df_cols(dt1,dt2)) 
        cols <- intersect(names(dt1),names(dt2))
        col1 <- setdiff(names(dt1),names(dt2))
        col2 <- setdiff(names(dt2),names(dt1))
        
        dt1x <- dt1[,.SD,.SDcols=cols] %>% t %>% as.data.table(keep.rownames = T)
        dt2x <- dt2[,.SD,.SDcols=cols] %>% t%>% as.data.table(keep.rownames = T)
        if(type==2){
                dtcomp <- cbind(fsetdiff(dt1x,dt2x),fsetdiff(dt2x,dt1x))
                dtcomp[,c(3):=NULL]
                setnames(dtcomp,c("parm",  "url1","url2"))
                print(dtcomp)
        } else 
                list(cols_x=col1,cols_y=col2)
}


# scrape the 600+ budget codes (code, pcode, description)
# 663 budget codes are scraped across 2014 till 2021
scrape_bud_codes <- function(url=url_loadtreegrid ){
        url %<>% param_set("pLevel",1) %>% param_set("pID",0) %>% param_set("pDateFrom","01-Apr-2014") %>%  param_set("pDateTo","31-Mar-2021")
        url %>% loop_url() -> dt_bh_wards
        dt_bh_wards[,pcode:=str_extract(name,"P\\d{4}")]
        dt_bh_wards[,code:=str_extract(id,"^\\d+") %>% as.numeric()]
        dt_bh_wards[,budhead:=str_remove(name,pcode) %>% str_trim]
        dt_bh_wards %<>% map_at(c(2,3,5,6,7),as.numeric) %>% as.data.table()
}

# scrape for a given budget code, ward wise counts and amounts - take the highest counts first
# use this is purrr map with multiple budget codes
scrape_bud_ward_pairs <- function(url=url_ward_bud,budcode){
        url %<>% 
                param_set("pID",budcode) %>% 
                param_set("pTreeGridMainID",7) %>% 
                param_set("pLevel",2) %>% 
                param_set("pLoad",0) %>% 
                param_set("pLast",0) %>% 
                param_set("pDateFrom","01-Apr-2014") %>%  
                param_set("pDateTo","31-Mar-2021")
        dt <- loop_url(url)
        dt[,pID:=str_extract(id,"\\d+-\\d+")]
}


# pass either a vector of wbids or a vector of Job  Numbers
# return will a DT with notings of accounts and CE  - one noting per row
scrape_file_notings <- function(wbid=NULL,Job=NULL,url=url_approval){
        if(!is.null(Job))  wbid <- scr_statusdata[jn %in% Job,wbid]
        dt_full <- data.table(0)
        url %<>% param_set("pAction","LoadGridApprovalLevels") 
        for (w in wbid){
                url %<>% param_set("pWorkBillID",w)
                dt <- url %>% jscrape()
                dt[,wbid:=w]
                dt[,dateremark:=dmy_hms(date)]
                dt_full <- rbind(dt_full,dt,fill=T)
        }
        dt_full[,wbid:=as.character(wbid)]
        dt <- scr_statusdata[dt_full,on="wbid"][!is.na(jn),.(jn,wbid,id,name,firstname,dateremark,remarks)]
        dt
}

# pending - history of commands geom_segment
visualize_delay <- function(){}


# use purr map and rbindlist to scrape all wards into one DT
scrape_ward_statistics <- function(ward=1,baseurl="http://bbmpwards2019.com/api/proposedWards/"){
        url <- paste0(baseurl,ward)
        dt <- url %>% read_lines %>% fromJSON %>% as.data.table() %>% .[1,c(1:9)]
        dt[,ward:=ward]
        w1[,c(2,3)][dt,on=.(id=ward)]
}

constr_tree_url <- function(url=urlb,treegrid=2,from="01-Apr-2019", to="30-May-2019",budhead=-1,ddoid=-1,id=0,last=0,level=1,load=0,action="LoadTreeGridData",transform=1){
                stopifnot(path(url) == path(urlb))
        url %>% 
                param_set("pAction",action) %>% 
                param_set("pTreeGridMainID",treegrid) %>% 
                param_set("pBudgetHeadID",budhead) %>% 
                param_set("pLoad",load) %>% 
                param_set("pDDOID",ddoid) %>% 
                param_set("pID",id) %>% 
                param_set("pTransform",transform) %>% 
                param_set("pDateFrom",from) %>% 
                param_set("pDateTo",to) %>% 
                param_set("pLast",last) %>% 
                param_set("pLevel",level)
        #param_remove("_")
        # param_remove("%24filter")
}

# work in progress
constr_status_url <- function(url=urla,action="LoadGridApprovalLevels",billid=175597){
                stopifnot(path(url) == path(urla))
        url %>% 
                param_set("pAction",action) %>% 
                param_set("pWorkBillID",billid)
}

# these set of functions are used to scrape approved projects from a three level data structure
scrape_new_projects <- function(start="01-Sep-2020",end= Sys.time() %>% format("%d-%b-%Y")){
        dt1 <- url_lev1_bud %>% param_set("pDateFrom",start) %>% param_set("pDateTo",end) %>% jscrape
        if(length(dt1) >3) {
                dt1[,bcode:=str_extract(id,"^\\d+") %>% as.numeric]
                cat("scraped: ",nrow(dt1)," budget codes \n")
                return(dt1$bcode %>% unique) 
        } else {
                message("No data scraped")
                return(integer(0))
        }
        
}

scrape_pairs <- function(start="01-Sep-2020",end= Sys.time() %>% format("%d-%b-%Y"),bcode=37){
        dt2 <- url_lev2_bud %>% param_set("pDateFrom",start) %>% param_set("pDateTo",end) %>% param_set("pID",bcode) %>% jscrape()
        if(length(dt2)>3){
                dt2[,bcode:=str_extract(id,"^\\d+")]
                dt2[,ward:=str_extract(id,"(?<=-)\\d+(?=-)")]
                dt2[,start:=dmy(start)][,end:=dmy(end)]
        } else {
                message("No data scraped")
                return(data.table(NULL))
        }
}

scrape_approved_job_details <- function(string_pair="37-17",start="01-Sep-2020",end= Sys.time() %>% format("%d-%b-%Y")){
        dt3 <- url_lev3_bud %>% param_set("pDateFrom",start) %>% param_set("pDateTo",end) %>% param_set("pID",string_pair) %>% jscrape()
        if(length(dt3)>3){
                dt3[,bcode:= str_extract(string_pair,"^\\d+") %>% as.numeric]
                dt3[,ward:=str_extract(string_pair,"(?<=-)\\d+$") %>% as.numeric]
                dt3[,start:=dmy(start)][,end:=dmy(end)][,approvedamount:=as.numeric(approvedamount)]
                setnames(dt3,"name","jn")
                dt3[,fy:=str_sub(jn,5,6) %>% as.numeric()]
                dt3[,pcode:=str_extract(budgethead,"P\\d{4}")]
                if(dt3[is.na(pcode),.N]>0) message("Detected NA Pcode in pair:",string_pair)
                dt3[!is.na(pcode),budgethead:=str_remove(budgethead,pcode) %>% str_trim]
                
        } else {
                message("No data scraped for ",string_pair)
                return(data.table(NULL))
        }
}

# The above 3 functions are used inside this.
scrape_all_approved_jobs <- function(start="01-Sep-2020",end= Sys.time() %>% format("%d-%b-%Y")){
        new_projects <- scrape_new_projects(start = start,end=end)
        new_proj_dt <- new_projects %>% map(~scrape_pairs(start = start,end= end, bcode = .x)) %>% rbindlist(fill = T)
        if(nrow(new_proj_dt)==0) {
                message("No new approved projects since ",start)
                return(NULL)
        }
        pairs <- new_proj_dt[,paste(bcode,ward,sep="-")]
        pairs %>% map(~scrape_approved_job_details(start=start,end = end,string_pair = .x)) %>% rbindlist(fill = T)
}

# url_payment is a hardcoded url
# returns DT with RTGS dates along with wbid and other BR dates for paid bills only
scrape_payments <- function(start="01-Sep-2020",end= Sys.time() %>% format("%d-%b-%Y")){
        h1 <- url_payment %>% param_set("pDateFrom",start) %>% param_set("pDateTo",end) %>% read_html
        rvest::html_nodes(h1,css = "p") %>% html_text() %>% fromJSON() %>% as.data.table
}

# this scrapes all JOBs (2 digit and 3 digits in 2 steps)
# remember to prefix 0 for 2 digit wards
# output is DT with job number as well as all bills under each job number
scrape_billstatus <- function(ward="ALL"){
        if(grepl("all",ward,ig=T)) {
        .url1 <- url_loadtypecombo %>% param_set("pJobNumber",0) # all 2 digit wards
        .url2 <- url_loadtypecombo %>% param_set("pJobNumber",1) # all 3 digit wards
        } else 
                .url1 <- url_loadtypecombo %>% param_set("pJobNumber",ward)
                
        h1 <- read_html(.url1)
        dt1 <- rvest::html_nodes(h1,css = "p") %>% html_text() %>% jsonlite::fromJSON() %>% as.data.table
        if(exists(".url2")) {
                h2 <- read_html(.url2)
                dt2 <- rvest::html_nodes(h2,css = "p") %>% html_text() %>% jsonlite::fromJSON() %>% as.data.table
                return(rbind(dt1,dt2))
        } else 
                return(dt1)
}

# process the master url scrape (start date to end date)
proc_payments <- function(dt){
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


#-------  WORKS BILL PUBLIC VIEW ---------- #

#scrape yr 
scrape_workbills <- function(srch="-17-",frm="01-Jan-1900",to="01-Jan-1900"){
baseurl <-  ("http://218.248.45.166:8092/PublicView/vss00CvStatusData.php?pAction=LoadPaymentGridData&pDateType=pDF&pOrderBy=-1&pFinancialYearID=-1&filterscount=0&groupscount=0&sort")
loaded_url <- baseurl %>% param_set("pCriteria",srch) %>% param_set("pDateFrom",frm) %>% param_set("pDateTo",to)
loaded_url %>% jscrape
}

 
#remove slno and add fy & ward
wbf1 <- function(x) x[,jn:=str_extract(wcname,"\\d{3}-\\d{2}-\\d{6}")
                      ][,ward:=str_sub(jn,1,3) %>% as.numeric
                        ][,fy:=str_sub(jn,5,6) %>% as.numeric
                          ][,slno:=NULL]


wb_unpack_jobcode <- function(str) str %>% map_chr(~read_html(.x) %>% html_text)
wb_unpack_brdet <- function(str) str %>% map_chr(~read_html(.x) %>% html_text)

wb_split_jobcode <- function(x){
        job <- str_extract(x,"\\d{3}-\\d{2}-\\d{6}")
        office <- str_extract(x,"(?<=Office\\s?:).+(?=Budget)") %>% str_trim
        budgetstr <- str_extract(x,"(?<=Budget\\s?:).+(?=Contractor)") %>% str_trim
        pcode <- str_extract(budgetstr,"P\\d{4}")
        budgethead <-  str_remove(budgetstr,pcode) %>% str_trim
        contractor <- str_extract(x,"(?<=Contractor\\s?:).+$") %>% str_trim
        data.table(jn=job,office=office,pcode=pcode,budgethead=budgethead,contractor=contractor)
} 

wb_split_brdetails <- function(x){
        fn <- function(dates=dates,nos=nos){
                fn2 <- function(str,var) {
                        ifelse(grepl(str,nos),var,NA) %>% na.omit %>% {ifelse(length(.)==0,NA_character_,as.character(.))}
                }
                wo_date= fn2("^Order",dates) %>% dmy
                sbr_date= fn2("^SBR",dates) %>% dmy
                br_date= fn2("^BR",dates) %>% dmy
                cbr_date= fn2("^CBR",dates) %>% dmy
                pay_date= fn2("^Payment",dates) %>% dmy
                wono= fn2("^Order",nos)
                sbrno= fn2("^SBR",nos)
                brno= fn2("^BR",nos)
                cbrno= fn2("^CBR",nos)
                payno= fn2("^Payment",nos)
                data.table(wodate=wo_date,sbrdt=sbr_date,brdt=br_date,cbrdate=cbr_date,paydate=pay_date,
                           wono=wono,sbrno=sbrno,brno=brno,cbrno=cbrno,payno=payno)
        }
        ld <- x %>% str_extract_all("\\d{2}-[JFMASOND][a-z]{2}-\\d{4}")
        ln <- x %>% str_extract_all("Order\\s?: .{6}|SBR\\s?: .{6}|BR\\s?: .{6}|CBR\\s?: .{6}|Payment\\s?: \\d{6}")      
        map2(ld,ln,fn) %>% rbindlist()
}

maxdate <- function(datev=NULL){
        if(is.infinite(max(datev,na.rm = T)))  return(NA_Date_) else return(max(datev,na.rm = T))
}

#====== EXCEL DOWNLOADED FROM PUBLIC VIEW ==========

read_pubexcel <- function(file){
        readxl::read_excel(file) %>% row_to_names(1) %>% clean_names() %>% select(!contains("in_words")) %>% setDT
}



# pass the DT from read
proc_dt_excel <- function(dt){
        split_work <- function(x){
                start_date <-   str_extract(x,"(?<=Work Started on )\\d{2}-...-\\d{4}") %>% dmy()
                end_date <- str_extract(x,"(?<=Work Ended on )\\d{2}-...-\\d{4}") %>% dmy()
                wdescr <- str_extract(x,"(?<=br/>).+")
                data.table(start_date=start_date,end_date=end_date,wdescr=wdescr)
        }
        #dt1 <- dt$name_of_work %>% split_work()
                dt[,wdescr:=str_extract(name_of_work,"(?<=br/>).+")]
        dt %<>% map_at(vars(contains("date")),dmy) %>% as.data.table()
        dt %<>% map_at(vars(matches(match = c("nett","gross","deduction"))),as.numeric) %>% as.data.table()
        dt[, start_date :=   str_extract(name_of_work,"(?<=Work Started on )\\d{2}-...-\\d{4}") %>% dmy()]
        dt[, end_date := str_extract(name_of_work,"(?<=Work Ended on )\\d{2}-...-\\d{4}") %>% dmy()]
        dt[,fy:=str_sub(job_number,5,6) %>% as.numeric]
        dt[,ward:=str_sub(ward,1,3) %>% as.numeric]
        setDT(dt)
        #cbind(dt,dt1)
}

# pass the DT obtained from above fn
sumrz_exceldt <- function(dt){
        dt[,.(totbillamt=sum(nett),lBilldt=last(.SD$sbr_date %>% sort),fWODate=first(.SD$order_date %>% sort),runBills=sum(ifelse(grepl("Running",bill_type),1,0)),Nbills=.N,contrname=first(contractor),mobile=first(mobile)),by=.(jn=job_number,fy=str_extract(job_number,"-\\d{2}-") %>% str_extract("\\d+") %>% as.numeric)]
}
