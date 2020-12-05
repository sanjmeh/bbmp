#library(plotly)
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
#library(summarytools)
library(data.table)
library(ggplot2)
library(jsonlite)
#load("imp.RData")

# for Sowmya
plot_garbage <- function(wastedt=w1){
ggplot(wastedt) + 
        geom_path(aes(year,value),size=0.5) + 
        geom_point(aes(year,value,color=variable),size=3) + 
        facet_wrap( ~variable,scales = "free_y",ncol = 1,labeller = labeller(variable=c(
                "waste" = "Tonnes Per Day Waste",
                "pop" = "Population in Millions",
                "pc" = "Waste Per capita")
        )) + xlab("YEAR") + 
        scale_color_discrete(name="Legend",labels=c("Waste","Population","Percapita Waste")) +
        ggtitle("BANGALORE WASTE GROWTH")
}

read_tables <- function(url_pdf=contr_civil1) extract_tables(url_pdf)

read_bbmp <- function() setDT(read_sheet(bbmp_fin,range = "A:M",col_types = "cccnnnncdDDDc")) %>% setnames(qc(jobn,desc,budhd,appr,grspnt,dedc,netspnt,contr,mob,strtdt,finshdt,paiddt,ccert))

# pass the output of read_bbmp here
procb <- function(dt){
        dt[,ward:= str_split(jobn,"-")[[1]][1],by=jobn]
        dt[,yr:= str_split(jobn,"-")[[1]][2] %>% paste0("20",.),,by=jobn]
        dt[,budcode:= str_extract(budhd,"^P\\d+"),,by=budhd]
}

# pass the ouptut of procb here
yoyperc <- function(dt){
        dt[,.(usepercent=percent(sum(grspnt,na.rm = T)/sum(appr))),by=.(budcode,yr)] %>% dcast(budcode ~ yr)
}

yoybudget <- function(dt){
        dt[,.(budget=round(sum(appr)/10^5,2),spent=round(sum(grspnt,na.rm = T)/10^5,2)),by=.(budcode,yr)] %>% 
                dcast(budcode ~ yr, value.var="budget")
}

yoyspend <- function(dt){
        dt[,.(spent=round(sum(grspnt,na.rm = T)/10^5,2)),by=.(budcode,yr)] %>% 
                dcast(budcode ~ yr, value.var="spent")
}


outtab1 <- function(dt){
        dt %>% 
                addHtmlTableStyle(css.total = c("border-top: 1px dashed grey;",
                                        "border-top: 1px dashed grey;",
                                        "border-top: 1px solid grey; font-weight: 900")) %>% 
              htmlTable()
        
}

#============= NEW FUNCTIONS to read in Intern Data on BBMP  =====
# read first few lines of some or all files (all text)
read_titles <- function(n=1:10,l=3,sheet=1,str=NULL,dir="BaseData"){
        fullpaths <- list.files(dir,full.names = T)
        if(!is.null(str))  # if a str is supplied only those files that match the pattern are picked up
                fullpaths <- grep(str,fullpaths,value = T,ig=T)
        if (grepl("ALL",n)[1] | !is.null(str)) 
                        n<- seq_along(fullpaths) # n will be replaced full range of paths
        dt <- 
                fullpaths[n] %>% 
                      imap(~read_excel(sheet = sheet,path = .x,n_max=l,col_types = "text",col_names = F),progress=T) %>% 
                rbindlist(idcol = "file",fill=T)
        fnames <- fullpaths %>% map_chr(~str_split(.x,"/") %>% unlist %>% tail(1))
        dtnames <- data.table(sn=seq_along(fnames[n]),fname=as.factor(fnames[n]),ward=str_extract(fnames[n],"^\\d+") %>% as.numeric)
        stopifnot(nrow(dt)>0)
        dtnames[dt,on=.(sn=file)]
}

# pass the raw excel DT (read using read_titles()) 
enrich <- function(dt){
        dt <- dt[,c(1:16)] %>% clean_jobno
        setnames(dt,
                 c("sn", "fname", "ward", "Job Number", "Description", "Budget Head", 
                   "Approved", "Gross Spent", "Deductions", "Net Spent", "Contractor", 
                   "Mobile", "Start", "Finish", "Paid", "Completion Certificate"
                 ))
        dt[,Appd_num:=as.numeric(Approved)]
        dt[,GSpent_num:=as.numeric(`Gross Spent`)]
        dt[,NSpent_num:=as.numeric(`Net Spent`)]
        dt[,deductions_num:=as.numeric(Deductions)]
        dt[,wardn:=str_extract(`Job Number`,  "\\d+(?=-)")]
        
        dt[grepl("na|nil",Paid,ig=T),Paid:=NA
        ][grepl("na|nil",Finish,ig=T),Finish:=NA
        ][grepl("na|nil",Start,ig=T),Start:=NA]
        
        tr_dates(dt,c("Paid","Finish","Start"))
        
        # dt[as.numeric(Paid) %>% between(32000,46000),Dat_paid:=excel_numeric_to_date(as.numeric(Paid))]
        # dt[as.numeric(Finish) %>% between(32000,46000),Dat_finish:=excel_numeric_to_date(as.numeric(Finish))]
        # dt[as.numeric(Start) %>% between(32000,46000),Dat_start:=excel_numeric_to_date(as.numeric(Start))]
        
        dt[,fy:=str_extract(`Job Number`,  "(?<=-)\\d+(?=-)")
        ][,wardn:=str_extract(`Job Number`,  "\\d+(?=-)")
        ][,jobsn:=str_extract(`Job Number`,  "(?<=-)\\d+$")
        ][,budgcode:=str_extract(`Budget Head`,"P\\d{4}")]
        
        dt[is.na(Dat_start) & !is.na(Start),Dat_start:=parse_date_time(Start,orders = c("dmy","mdy")) %>% as.Date()]
        dt[is.na(Dat_paid) & !is.na(Paid),Dat_paid:= parse_date_time(Paid,orders = c("dmy","mdy"))]

        
        dt <- dt[!grepl("TOTAL",`Budget Head`)
                 ][!grepl("Approved",Approved)
                   ][!grepl("FY",`Job Number`)
                     ][!grepl("job",`Job Number`,ig=T)
                       ][!grepl("paid|pay date",Paid,ig=T)
                         ]

        dt <- remv_rows() # remove NA Job rows and NA amounts - tinker with parameters if you donot want NA Job Number to be removed
        
        dt1 <- budg_u_names(dt)[dt,on="budgcode"] # replace budget heads with unique budget heads for each budget code
        dt1 %>% map_at(.at = c("budgcode","budgname","cmpl_cert","fy","jobsn","wardn"),.f = as.factor) %>% as.data.table
}

tr_dates <- function(dt,datecols=c("paid","start","finish")){
  seq_along(datecols) %>%  walk(~dt[as.numeric(get(datecols[.x])) %>% between(32000,46000),paste("Dat",datecols[.x],sep = "_"):=excel_numeric_to_date(as.numeric(get(datecols[.x])))] )
}


# pass a directory with files that have prefixed number. The number is a string if we sort directly but the function will enabke a numeric sort
sort_files <- function(dir="BaseData"){
        fnames <- list.files(dir,full.names = F) 
        wno <- fnames %>% str_extract("^\\d+") %>% as.numeric
        dt <- data.table(names=fnames,ward=wno)
        dt[order(ward),names]
}

# identify first lines now from output of above but with l=Inf, so it has all data now.
outp_1st <- function(dt){
        dt[,{.SD[1]},by=fname]
}

# Known corruption is hardcoded.. cannot be used on new set of files
find_corrpt_file <- function(dt){
        f1 <- outp_1st(dt)[grepl("AREA",`...4`),fname] %>% unique %>% as.character()
        f2 <- outp_1st(dt)[grepl("Sl",`...1`),fname] %>% unique %>% as.character()
        c(f1,f2)
}

# returns the same DT with Job Number cleaned up - perhaps
clean_jobno <- function(dt){
        dt[nchar(`Job Number`)>13,`Job Number` := str_sub(`Job Number`,1,13)]
}

# remove one column (as named in the parameter) and name the file (the corrupt_fname).
# use this function recursively - one corrupt file at a time.
# donot forget to pass the corresponding column name that needs to be removed.
remv_col <- function(dt,corrpt_fname,col_to_remv="...1"){
        colnames <- names(dt) %>% setdiff(col_to_remv)
        smalldt <- dt[fname %in% corrpt_fname,.SD,.SDcols=colnames]
        largedt <- dt[!fname %in% corrpt_fname][,c(1:16)]
        if(length(smalldt) > 16) smalldt <- smalldt[,c(1:16)]
        if(length(smalldt) < 16) {
                message("Warning: One less column detected, Padding with NAs:",corrpt_fname)
                smalldt <- cbind(smalldt,dummy=NA)
        }
        rbindlist(list(largedt,smalldt),use.names = F)
}

# remove junk rows that have no meaning - keep adding to the list
# this is now called from inside the enrich function
remv_rows <- function(dt,rm_na_amts=T,rm_na_job=T)
{
        if(rm_na_job) dt <- dt[!is.na(`Job Number`)] 
        if(rm_na_amts) dt <- dt[!(is.na(`GSpent_num`) & is.na(`NSpent_num`) & is.na(`Appd_num`))] 
        dt
}

# pass the master dt with atleast 2 budget columns
# this is now called from inside the enrich function
budg_u_names <- function(dt){
        dt[,.(budgcode,`Budget Head`)] %>% unique -> dtbudhds
        dtbudhds[,clean_head:=first(`Budget Head`) %>% str_remove("P\\d{4}") %>% str_trim,by=budgcode]
        dtbudhds[!is.na(budgcode),.(budgcode,budgname=clean_head)][order(budgcode)] %>% unique
}

clust_names <- function(dt){
        dt[is.na(budgcode),.(budgcode,`Budget Head`,Description)] %>% unique
}

add_ccert <- function(dt){
        dt[,cmpl_cert:=case_when(
                grepl("Yes|Y$",`Completion Certificate`,ig=T) ~ "YES",
                grepl("No|N$",`Completion Certificate`,ig=T) ~ "NO",
                is.na(`Completion Certificate`) ~ NA_character_,
                grepl("NA|without data",`Completion Certificate`,ig=T) ~ NA_character_,
                !is.na(`Completion Certificate`) ~ "Others"
        )]
}

# output will the same DT with the column Job Number replaced with the new codes that match up with old codes as well as budget head (both must match)
# needed to clean intern data, not on scraped data
repl_jobno <- function(dt1){
        jobdt <- fread("newjobnos.csv")
        copy(dt1) -> dt
        stopifnot(length(jobdt)==3)
        setnames(jobdt,qc(budgcode,oldjob,newjob))
        dt2 <- jobdt[dt,on=.(budgcode,oldjob=`Job Number`)
        ][,`Job Number`:=ifelse(is.na(newjob),oldjob,newjob)
        ][,newjob:=NULL][,oldjob:=NULL
        ][,fy:=str_extract(`Job Number`,  "(?<=-)\\d+(?=-)")
        ][,wardn:=str_extract(`Job Number`,  "\\d+(?=-)")
        ][,jobsn:=str_extract(`Job Number`,  "(?<=-)\\d+$")]
        setcolorder(dt2,names(dt1))
        dt2
}

# on intern data but may work on all
krid_dash <- function(dt,pat="KRIDl|rural infra|technical man|executive"){
        dt[fy>16,.(exp_KRID=sum(ifelse(grepl(pat,Contractor,ig=T), NSpent_num,0),na.rm = T)/1e7,
                   Tot_exp=sum(NSpent_num,na.rm = T)/1e7,
                   Count_KRID=sum(grepl("KRIDl|rural infra|technical man|executive",Contractor,ig=T),na.rm = T),
                   Count_TOT=.N),by=.(fy,ward)
        ][,perc_amt:=percent(exp_KRID/Tot_exp)
          ][,perc_Nproj:= percent(Count_KRID/Count_TOT)]
}

# simplified
approval_rep <- function(dt){
        # appd_dt <- dt[,.SD[,.(Nonz=sum(Appd_num > 0,na.rm = T),Totrows=.N)],by="Job Number"] # nonzero rows vs total rows per JOB
        # Appd_Amts <- appd_dt[Nonz>=1,`Job Number`] %>% {dt[`Job Number` %in% .][,.(appd_amt=max(Appd_num,na.rm = T)),`Job Number`]}
        Appd_Amts <- dt[Appd_num>0,.(appd_amt=max(Appd_num,na.rm = T)),`Job Number`]
        Appd_Amts[order(`Job Number`)
        ][,fy:=str_extract(`Job Number`,  "(?<=-)\\d+(?=-)")
        ][,ward:=str_extract(`Job Number`,  "\\d+(?=-)") %>% as.numeric()]
}

isvalid_mob <-  function(str) {
        str <- ifelse(is.na(str),"",str)
        str <- str_trim(str)
        ifelse(str_detect(str,"[6-9][0-9]{9}$"),T,F)
}

pull_mobile <- function(dt,check='Contractor',move="right"){
        dt[,Mobile:=ifelse(isvalid_mob(get(check)) & !isvalid_mob(Mobile),get(check),Mobile)]
}

pull_date <- function(dt,check='Contractor'){
        dt[,Date_value:=ifelse(isvalid_dt_num(get(check)),get(check),NA)]
}

isvalid_dt_num <- function(str,from_date=32000,to_date=45000){
        str <- suppressWarnings(as.numeric(str))
        str <- ifelse(is.na(str),0,str)
        ifelse(between(str,from_date,to_date),T,F)
}

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
        
get_bud_clust <- function(dt,ht=2){
        dt <- dt[!is.na(budgname)]
        clustbud <- clust_addr(ht,dt,var = "budgname")
         code_lookup <- clustbud[!is.na(clust),.(string,clust,budgcode)][order(clust)] %>% unique
         code_lookup[,poss_codes:=ifelse(is.na(budgcode), unique(budgcode) %>% na.omit %>% paste(collapse= "/"),budgcode),by=clust]
}

# ward_names DT must exist as it is joined
prep_plot <- function(dt){
        setDT(dt)
        dt <- ward_names[dt,on="ward"]
        dt[,wrd_fct:=factor(ward,levels = unique(ward),labels = unique(ward_name),ordered = T)]
        dt[,delay:=Dat_paid - Dat_finish]
        dt[,Mdelay:=median(delay,na.rm = T),.(fy,ward)]
        dt[,tot_nspent:=sum(NSpent_num,na.rm = T),.(fy,ward)]
        dt[,clean_amt:=sum(ifelse(is.na(delay),0,NSpent_num)),.(fy,ward)]
        dt[,yr_strt:=year(Dat_start)]
        dt[,yr_finsh:=year(Dat_finish)]
        dt[,yr_paid:=year(Dat_paid)]
        dt[,tot_proj:=.N,.(fy,ward)]
        dt[!is.na(Dat_finish),execyr:=-as.numeric(paste0("20",fy)) + year(Dat_finish)]
        # assuming year of payment is year of execution when Finish date is missing:
        dt[is.na(Dat_finish) & !is.na(Dat_paid),execyr:=-as.numeric(paste0("20",fy)) + year(Dat_paid)] 
        dt[,payyr:=factor(case_when(execyr==0 ~ "0: SAME YEAR",
                                    execyr ==1 ~ "1: NEXT YR", 
                                    execyr==2 ~ "2: NEXT TO NEXT YR",
                                    execyr==3 ~ "3: YEAR 3",
                                    TRUE ~ "NO EXEC DATE"))]
        dt[,flag:=ifelse(is.na(Dat_paid) & !is.na(Dat_finish),"FINSH & NOTPAID",
                         ifelse(is.na(Dat_paid) & is.na(Dat_finish), "NOTFIN & NOTPAID",
                                ifelse(!is.na(Dat_paid) & !is.na(Dat_finish), "FINSH & PAID", "NOTFIN & PAID" )))]
        
}

# clever way to remove drag error in any column that needs to be one value at the top
rm_rept_appr <- function(dt,var="Appd_num"){
        dt[,amt2:=ifelse(is.na(get(var)),0,get(var))]
        dt[,amt2:=max(amt2),`Job Number`]
        dt[,amt2:=c(first(amt2),diff(amt2)),`Job Number`]
}

# manual corrections to values (entered on googlesheet)
# make sure jn is changed back to `Job Number`
# also make sure you pass dt with budgcode, and Appd_num columns existing
manual_correct <- function(dt){
        dt[budgcode %in% c("P0375"),budgcode:="P3075"] # budgetcode incorrect 
        err_jobs1 <- c("118-17-000016","171-18-000024") # these jobs have incorrect approval amounts
        err_jobs2 <- c("147-17-000065") # this job has incorect netspent
        err_jobs3 <- c("118-17-000025") # this job has a fake additional entry with 0 approved amount combo
        appramt <- c(10.65e5,23e5) # new approval amounts
        netsp <- c(18.36e5) # new netspent amount
        dt[`Job Number` %in% err_jobs1 ,Appd_num:=appramt] # replace incorrect approved amounts
        dt[`Job Number` %in% err_jobs2 ,NSpent_num:=netsp]
        dt <- dt[!(`Job Number` %in% err_jobs3 & is.na(Appd_num))] # remove the extra job number
        repl_jobno(dt) # replace 17 job numbers
}

mean2 <- function(x) mean(x,na.rm = T) %>% as.numeric %>% round_half_up
median2 <- function(x) median(x,na.rm = T) %>% as.numeric %>% round_half_up()

hist_proj <- function(dt,years=17:20,var="proj_days",title="Project Days",yaxis="Project Counts"){
        dt1 <- dt[fy %in% years]
        ymax <-  dt1[get(var) %in% 1:1000,get(var)] %>% as.numeric %>% hist(breaks=30*(0:40),xlim = c(0,1000),) %>% .$counts %>% max
        dt1 %>% ggplot + 
                geom_histogram(aes(get(var)),binwidth = 30) + xlim(c(0,1095)) + 
                geom_vline(xintercept = c(365,mean2(dt1[,get(var)]),730),lty=c(2,3,2),size=0.5) + 
                annotate("rect",xmin = 365*(1:3) - 185, ymin = ymax*0.54,xmax=365*(1:3) - 80,ymax=ymax*0.66,fill="dodgerblue1")  + 
                annotate("text",x = 365*(1:3) - 137, y = ymax*0.6,
                         label=c(dt1[,.(sum(get(var)<365,na.rm = T)/(sum(!is.na(get(var)))),
                                        sum(between(get(var),365,730),na.rm = T)/sum(!is.na(get(var))),
                                        sum(between(get(var),730,1090),na.rm = T)/sum(!is.na(get(var))))] %>% 
                                         as.numeric %>% percent()),color="white",size=6) + 
                annotate("text",x = 365*(1:3) - 180,y=ymax,label=paste("Year-",1:3),size=10) + 
                annotate("text",x=dt1[,get(var)]%>% mean2 + 40,y=ymax*0.9,label=paste("Mean:",dt1[,get(var)] %>% mean2,"days"), size=4) +
                xlab(label = "No. of Days") + 
                ylab(label = yaxis) +
                ggtitle(title,paste("Years:",paste(years,collapse = ",")))
        
        
}



edit_url_date_range <- function(url,strt="01-Sep-2020",end="31-Mar-2021"){
  str_replace(url,"DateFrom=\\d{2}-...-\\d{4}&pDateTo=\\d{2}-...-\\d{4}",
              paste0("DateFrom=", strt, "&pDateTo=",end)
  )
}

edit_url_ID <- function(url,id){
  str_replace(url,"pAction=LoadTreeGridData&pLoad=\\d&pID=\\d{4}",
              paste0("pAction=LoadTreeGridData&pLoad=\\d&pID=",id)
  )
}

edit_job_url <- function(url=jobnumber_url,jobno_str="046-20"){
  str_replace(url,"JobNumber=1",paste0("JobNumber=",jobno_str))
}


# idt is the interns DT and sdt is the scraped DT, nm = nomatch treatment
# output is merger summary
jobs_pdt <- function(idt,sdt,nm=0,flip=F){
  dt1 <- idt[,.(netspend=sum(NSpent_num,na.rm = T),paid_list=list(dates=na.omit(Dat_paid)),i.N=.N),jn]
  dt2 <- sdt[,.(netpaid=sum(nett,na.rm = T),rtgs_list=list(dates=Dat_rtgs),s.N=.N),jn]
  if(flip==T)
    dt3 <- dt2[dt1,on="jn",nomatch=nm][,diff:=netpaid - netspend][,Nrtgs:=length(rtgs_list[[1]]),jn][,Npaid:=length(paid_list[[1]]),jn] 
  else
  dt3 <- dt1[dt2,on="jn",nomatch=nm][,diff:=netpaid - netspend][,Nrtgs:=length(rtgs_list[[1]]),jn][,Npaid:=length(paid_list[[1]]),jn]
  
  # dt3[,urtgsN:=rtgs_list[[1]] %>% uniqueN,jn]
  # dt3[,upaidN:=paid_list[[1]] %>% uniqueN,jn]
  dt3[lengths(rtgs_list) != lengths(paid_list),status:="SIZE_MISMATCH"]
  dt3[,status:=fifelse(lengths(paid_list)==0, "MISSED",status)]
  dt3 <- dt3[is.na(status),status:=fifelse(allmatch(rtgs_list,paid_list),'MATCHES',status),jn]
  #dt3 <- dt3[Npaid>0][,status:=fifelse(allmatch(rtgs_list,paid_list),"MATCH",status)]
  #dt3[,diffdays:=ifelse(length(rtgs_list[[1]])>0, last(rtgs_list[[1]],na.rm = T) - last(paid_list[[1]],na.rm = T),NA),jn]
  # dt3[upaidN>0 & urtgsN!=upaidN & all(paid_list[[1]] %in%  rtgs_list[[1]]),status:="SUBSUMED",jn]
  # dt3[upaidN>0 & urtgsN!=upaidN & all(rtgs_list[[1]] %in% paid_list[[1]]) & status!="SUBSUMED",status:="EXTRA_DATES",jn]
  dt3[,fy_appr:=str_sub(jn,5,6) %>% as.numeric]
  dt3[,rtgs_dates:=rtgs_list[[1]] %>% unique %>% paste(collapse = ", "),jn]
  dt3[,paid_dates:=paid_list[[1]] %>% unique %>% paste(collapse = ", "),jn]
}

allmatch <- function(dat1,dat2){
  fall <- function(x,y) if (length(x)==length(y)) all(x==y) else FALSE
  if(length(dat1[[1]]) !=length(dat2[[1]])) return (FALSE)
  x <- map(dat1,sort) %>% unlist
  y <- map(dat2,sort) %>% unlist
  map2_lgl(x,y, ~fall(.x,.y) %>% all) %>% all
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



# read intern googlesheet with corrected dates downloads and returns a DT
read_zerodays <- function(){
 gs1 <-   read_sheet("https://docs.google.com/spreadsheets/d/1Rte3pN_iqkk6nZ5y-_A6InqQPYmlHgMPwdKfMXj74jg/edit#gid=1412046666",sheet="Zero_days",range = "A1:P7007",col_types = "cc-D--------cDcD") %>% setDT
 setnames(gs1,qc(wbid,jn,wodt,wmatch,hcwdt,bmatch,bcrdt))
 gs1[,hcwdt:=as.Date(hcwdt)]
  gs1[,bcrdt:=as.Date(bcrdt)]
}

# Returns a two column DT with job number and hand edited date. Also removes duplicated rows from intern sheet. Pass the column name that you want the hand edited DT for, so you get WO dates or Bill creation dates.
extract_handedits <- function(dt,var="hcwdt"){
  dt[,wbid:=as.numeric(wbid)]
  x1 <- dt[,.(wbid,jn)] %>% duplicated(fromLast=T) %>% which %>% {-1*.} %>% {dt[.,.(wbid,jn,get(var))][!is.na(V3)]}
  setnames(x1,"V3",var)
}

# this will transform scraped data to the citizen portal data without keywords. 
# pass the raw scraped DTs as follows:
# scrape_billstatus() -> dt_billstatus_allwards
# scr_rtgs_dt <- scrape_payments()

prep_citzport <- function(kpat="krid|technical mana|executive eng|Rural Infr|k\\.r\\.i\\.d\\.l",
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

# read the scraped files - takes care of duplicates
read_scraped_files <- function(pat="",fromdate="01-Nov-2020"){
    frdate= dmy(fromdate)
    files <- list.files(path = "scraped_bills",pattern = pat,full.names = T) %>% file.info() %>% as.data.table(keep.rownames = T)
    ffiles <- files[mtime>=frdate & size>0] # filtered files
    fnames <- ffiles$rn
    names(fnames) <- fnames
    dt <- fnames %>%  imap(~fread(.x) %>% cbind(.y)) %>% map_depth(.depth = 2,as.character) %>% rbindlist(fill = T) # master tip for converting all to character to avoind error:  Internal error: column 3 of result is determined to be integer64 but maxType=='character' != REALSXP
    
    dt2 <- dt[ffiles[,.(rn,mtime)],on=.(.y=rn)]
    dt3 <- dt2[order(wbid,mtime)] %>% setkey(wbid)
    dt4 <- unique(dt3,fromLast = T,by = "wbid")
    dt4
}

# process new files (scraped by scrape_loop function) and rbind the output to scr_bills_master DT for unique wbids
proc_scr_columns <-  function(dt=NULL,kpat= "krid|technical mana|executive eng|Rural Infr|k\\.r\\.i\\.d\\.l"){
  dt[,kridl:=grepl(kpat,contractorname)]
  dt[,wbid:=as.numeric(wbid)]
  dt %<>% map_at(.at = c("deduction","gross","nett"),as.numeric) %>% as.data.table
  dt %<>% map_at(.at = c("cbrdate","dbrdate","sbrdate"),parse_date_time,order="ymd") %>% as.data.table
  dt %<>% map_at(.at = c("rtgsdate"), parse_date_time,order="dmy") %>% as.data.table
  dt %<>% map_if(.p = is.POSIXct, ~fifelse(.x<ymd(20000101),NA_Date_,as.Date(.x))) %>% as.data.table
  if("budget" %in% names(dt)){
  dt[,pcode:=str_extract(budget,"P\\d{4}")]
  if(dt[is.na(pcode),.N]>0) message("Detected NA Pcode in",dt[is.na(pcode),.N],"rows.")
  dt[!is.na(pcode),budgethead:=str_remove(budget,pcode) %>% str_trim]
  }
  dt[,c("grossinwords","nettinwords","deductioninwords","budget"):=NULL]
}

# merges incremental scrapes into the 
# make sure both dt1, dt2 have the mtime column 
merge_mult_wbids <- function(dt1,dt2=scr_bills_master_u){
  dt1[,wbid:=as.numeric(wbid)]
  dt2[,wbid:=as.numeric(wbid)]
  dt3 <- rbind(dt1,dt2,fill=T)
  dt4 <- dt3[order(wbid,mtime)] %>% setkey(wbid)
  dt5 <- unique(dt4,fromLast = T,by = "wbid")
  dt5
}

# Citizen Portal ward summary generation
genr_summ_citzport <- function(){
  jobs_citzport[,.(ProjAppr=.N,
                   ProjImpl=sum(ifelse(is.na(WorkOrderDate),0,1)),
                   ApprAmt=sum(BudgetAmount)/1e7,
                   ImplAmt=sum(ifelse(is.na(TotalExpense),0,TotalExpense))/1e7
                   ),by=.(FY_Budget,Ward)]
}

# summary report on payment days / unpaid bills percent FY wise / budget head category wise
# add more mbudget head cateogy if needed
bill_report <- function(fullState=T){
  bills_master %>% left_join(master_approval[,.(jn,budgethead)]) %>% 
    {.[,.(projects=.N,avgpaydays=median(rtgsdate - sdt,na.rm = T),unpaidProjects=sum(ifelse(is.na(rtgsdate),1,0))),.(budgethead,fy)
    ][,budcat:=case_when(
      fullState & grepl("14th|State Finance",budgethead,ig=T) ~ "StateFin",
      !fullState & grepl("14th",budgethead,ig=T) ~ "StateFin",
      #grepl("\\bCM\\b",budgethead,ig=T) ~ "CMNgrOthana",
      grepl("\\bEWS\\b|\\bSC\\b|backward",budgethead,ig=T) ~ "EWSAreas",
      T ~ "Others"
      )
    ][,.(projects=sum(projects),Unpaid=sum(unpaidProjects),Avgpaydays=mean(avgpaydays,na.rm = T)),.(budcat,fy)
      ][,prc_unpaid:=percent(Unpaid/projects)] %>% as_tibble()
      }
}
# check trends of any type of project by modifying the case_when
trend1 <- function(){
  bills_master %>% left_join(master_approval) %>% 
    {.[,budcat:=case_when(
      grepl("\\bCM\\b",budgethead,ig=T) ~ "CMNgrOthana",
      grepl("\\bEWS\\b|\\bSC\\b|backward",descr,ig=T) ~ "EWSAreas",
      T ~ "Others"
    )
    ][,.(projects=.N,Unpaid=sum(ifelse(is.na(rtgsdate),1,0)),budget=round(sum(approvedamount,na.rm = T)/1e7,2),totexp=round(sum(nett,na.rm = T)/1e7,2)),.(budcat,fy)
    ][,prc_unpaid:=percent(Unpaid/projects)]
    }
}

# html report on top 20 wards in terms of approved amounts in FY 2020
top20 <- function(topn=20){
  ward_names <- readxl::read_excel("Corporators_Contacts.xlsx") %>% select(1:2) %>% setDT %>% setnames(qc(ward,ward_name))
  master_approval[ward<200 & fy>13,.(totamt=round(sum(approvedamount)/1e7,1)),.(ward,fy)][order(ward)] %>% dcast(ward~fy,value.var="totamt") %>% .[order(-`20`)] %>% head(topn) %>% left_join(ward_names) %>% setcolorder(c("ward","ward_name")) %>% htmlTable(rnames = F,align="rlrrrrrrr",css.cell="padding-left:0.5em",caption = "All amounts in Rs Crores")
}

# pass a character vector of mobile numbers. All invalid numbers wll be replaced by NA.
clean_mobile <- function(x){
  x %>% str_remove("^(990{8}|9{9}|8{9}|7{9})") %>% str_extract("[7-9][0-9]{9}")
}

# used to select the most used mobile for a contractor; pass contractor vector and mobile vector; returns a DT
replace_wth_popular <- function(dt){
  #dt <- data.table(name=name,value=value)
  dt[,{
    topval <- .SD[,.N,value][order(-N)][1:2,value] # take the top 2 popular values
    if(is.na(topval[1])) pval <- topval[2] else pval <- topval[1] # popular value cannot be NA
    #.SD[value==pval,.(popval=value)]
    .(pval)
  },name] %>% unique
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

plot_trend_proj <- function(dt=citzport_test,w=151,keypat="Drain"){
  types <- dt[ward==w & grepl(keypat,keywords,ig=T)] %>% cSplit("keywords",direction = "tall") %>% .[,keywords] %>% unique
  wname <- ward_names[ward==w,ward_name]
  pdata <- dt[ward==w & grepl(keypat,keywords,ig=T),.(appr=sum(approvedamount)/1e7,Expenses=sum(totbillamt,na.rm=T)/1e7),fy][order(fy)]
  pdata[,`Unspent Budget`:=appr-Expenses]
  pdata %>% 
    melt(id.var="fy",variable.name="Amount") %>% 
    filter(Amount!="appr") %>% 
    mutate(Amount=factor(Amount,ordered = T,levels = c("Unspent Budget","Expenses"))) %>% 
    ggplot(aes(fy,value)) + geom_col(aes(fill=Amount),position = "stack") + 
    scale_fill_manual(values = c("Unspent Budget"="grey",Expenses="steelblue2")) + 
    ggtitle(paste(wname," - ","Ward:",w),paste("Expenses on:",paste(types,collapse = ", "))) + xlab("FY") + ylab("Rs Crores")
}


# ward_master DT has can be read with an fread to bbmp_ward_master.csv
ward_ranks <- function(dt=citzport_test,wmaster=ward_master,yr=16:20,keypat="Foot"){
  setnames(wmaster, old ="ward_no",new = "ward",skip_absent = T)
  
  
  if(nchar(keypat)==0 | is.na(keypat)) 
    wsum <- dt[fy %in% yr,.(totapp=sum(approvedamount,na.rm = T),totexp=sum(totbillamt,na.rm = T)),.(ward)]
  else
    wsum <- 
      dt[fy %in% yr & grepl(keypat,keywords),.(totapp=sum(approvedamount,na.rm = T),totexp=sum(totbillamt,na.rm = T)),.(ward)]
  
  wsum %<>% left_join(wmaster,by="ward") %>% arrange(ward)
  
  setDT(wsum,key="ward")
 
  wsum <- 
    wsum[,.(totapp=sum(totapp) %>% round_to_fraction(denominator = 4),
            totexp=sum(totexp) %>% round_to_fraction(denominator = 4), 
            exp_pkm=(sum(totexp)/road_length_kms) %>% round_half_up(),
            exp_psqkm=(sum(totexp)/area_sq_km) %>% round_half_up(),
            exp_pcap=(sum(totexp)/population_2011) %>% round_half_up(),
            bud_pkm=(sum(totapp)/road_length_kms) %>% round_half_up(),
            bud_psqkm=(sum(totapp)/area_sq_km) %>% round_half_up(),
            bud_pcap=(sum(totapp)/population_2011) %>% round_half_up(),
            bud_ut=(sum(totexp)/sum(totapp)) %>% percent()
            ),
         .(ward,ward_name,road_length_kms,area_sq_km,population_2011,lakes_number,parks_number,playgrounds_number,govt_schools_number,bus_stops_number, street_lights_number)]
  
  wrank <- 
    wsum[order(-bud_pkm)][,rank_bud_pkm:=seq_along(ward)
    ][order(-exp_pkm)][,rank_exp_pkm:=seq_along(ward)
    ][order(-road_length_kms)][,rank_road_len:=seq_along(ward)
    ][order(-area_sq_km)][,rank_area:=seq_along(ward)
    ][order(-totapp)][,rank_totapp:=seq_along(ward)
    ][order(-totexp)][,rank_totexp:=seq_along(ward)
    ][order(-bud_ut)][,rank_bud_ut:=seq_along(ward)
    ][order(-bud_pcap)][,rank_bud_per_cap:=seq_along(ward)
    ][order(-exp_pcap)][,rank_exp_per_cap:=seq_along(ward)
    ][order(-bud_psqkm)][,rank_bud_psqkm:=seq_along(ward)]
  wrank
}

# slight modification of prev ranking. This one takes a custom sring to be applied in description over and above the keyword
ward_ranks_item <- function(dt=citzport_test,pat="Street",k="Light",yr=16:20,wnames=wards){
  wsum <- 
    dt[fy %in% yr & grepl(pat,description,ig=T) & grepl(k,keywords,ig=T),.(totapp=sum(approvedamount)/1e7,totexp=sum(totbillamt,na.rm = T)/1e7),.(ward)]
  wsum <- wsum[order(-totapp)][,rank_appr:=seq_along(ward)]
  wsum <- wsum[order(-totexp)][,rank_exp:=seq_along(ward)]
  wsum %>% left_join(wnames,by=c(ward="Ward No")) %>% setcolorder(c("ward","Ward Name"))
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


scatter1 <- function(dt=citzport_test,yr=16:20,wdet=ward_master,perc=0.2){
  wsum <-  ward_ranks(dt=dt,yr = yr,wmaster = wdet)
  wsum[,ward_display:=ifelse(bud_ut<perc | bud_ut>1.2,ward_name,NA)]
  wsum[,extreme_wards:=ifelse(bud_ut< perc | bud_ut>1.2,"ABN","NORM")]
  wsum %>% 
    ggplot(aes(totapp,totexp)) + 
    geom_point(aes(color=extreme_wards)) + 
    scale_color_manual(values = c(ABN="hotpink",NORM="grey")) +
    geom_abline(slope = 1,lty=2,col="red") +
    ggrepel::geom_text_repel(aes(totapp,totexp,label=ward_display))
    
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


# Histograms of ward counts with focus on one ward
plot_hist <- function(citzport_data = citzport_test, wardno=150,kpat="Foot",yr=16:20,var="bud_pcap",clip=0.99){
  to_crore <- function(amt) (amt/1e7) %>% round_to_fraction(4)
  
  dt_axis <- fread("axis_titles.csv")
  Keywords <- citzport_data$keywords %>% str_split(",") %>% unlist %>% str_trim %>% unique %>% str_subset(kpat) %>% paste(collapse =",")
  Years <- yr %>% paste(collapse =",")

  dt <- ward_ranks(dt = citzport_data, keypat = kpat,yr = yr,wmaster = ward_master)
  dt <- dt[get(var)<=clip*max(get(var))]
  wname <- dt[ward==wardno,ward_name]
  box_text <-  c(wname,Keywords,dt_axis[colname==var,axisname]) %>% paste(collapse ="\n")
  ylim <- hist(dt[[var]],plot=F,breaks = 30)$counts %>% max # cool way to find the scales of a histogram for label positioning
  xlim <- hist(dt[[var]],plot=F,breaks = 30)$mids %>% max
  meanval <-  mean(dt[[var]])
  wardval <- dt[ward==wardno,get(var)]
  ratio <- wardval/meanval
  statement <- paste("Ward",wname,"is",percent(ratio),"of the mean across all wards")
                   
  gplot <- dt %>% 
    ggplot(aes(get(var))) + 
    geom_histogram() + 
    geom_vline(xintercept = meanval,lty=2) + 
    geom_vline(xintercept = wardval,color="hotpink",lty=4,size=1.5) +
    geom_label(x=wardval,y=ylim*0.75,
               label=wname %>% str_split("\\s+",simplify = T) %>% unlist %>% paste(collapse = "\n")) + 
    geom_label(x=mean(dt[[var]]),y=ylim*0.33,label="Mean \nacross \nall wards") + 
    ggtitle(paste0("Historgram of ward counts for year",(ifelse(length(yr)>1,"s","")),":",Years),
            subtitle = paste("How",wname,"compares with other wards on budget heads:",Keywords)) + 
    xlab(dt_axis[colname==var,axisname])
  rect <- annotate("rect", xmin = 0.5*xlim,ymin=0.48*ylim, xmax=0.95*xlim,ymax=0.6*ylim,fill="dodgerblue") 
  numb <- annotate("text", label=percent(ratio),x=0.51*xlim,y=0.55*ylim,col="yellow",size=14,hjust ="left")
  sentence <- annotate("text", label="of the mean\nof all wards.",x=0.73*xlim,y=0.55*ylim,col="white",size=6,hjust =0)
  box2 <-  annotate("label",x=0.5*xlim,y=0.9*ylim,label=wname,hjust=0.5,size=13,fill="yellow",color="red")
  caption <- paste("This histogram is plotted across all 198 wards")
  gplot + rect + numb + sentence + box2
}

# Histograms of ward counts with high variance boxed out
# pass the vartype = "perc" for budget utilisation. 
hist_sigma <- function(citzport_data = citzport_test,kpat="Foot",yr=16:20,var="bud_pcap",vartype="",lsigma=-1,rsigma=2,totbins=30,perlabel="per capita",rsunit="",
                       Boxtitle="DRAINS",xstart=0,ystart=0){
  # change to lakhs or crores, e.g. u="crore"
  chng_unit <- function(value,u) {
    div=case_when(u=="crore" ~ 7,u=="lakh" ~ 5, u=="thousand" ~ 3, T ~ 0)
    out <- (value/(10^div)) %>% round_to_fraction(4)
    if(vartype=="perc") out <- percent(value,digits = 0)
    out
  }
  prfx <- ifelse(rsunit=="","","Rs")
  ushort <- case_when(rsunit=="crore"~ "cr", rsunit=="lakh" ~ "L",rsunit=="thousand" ~ "K", T ~"")
  pwr <- case_when(rsunit=="crore" ~ 7,rsunit=="lakh" ~ 5, rsunit=="thousand" ~ 3, T ~ 0)
  dt_axis <- fread("axis_titles.csv")
  Keywords <- citzport_data$keywords %>% str_split(",") %>% 
    unlist %>% str_trim %>% unique %>% str_subset(kpat) %>% paste(collapse =",")
  Years <- yr %>% paste(collapse =",")

  dt <- ward_ranks(dt = citzport_data, keypat = kpat,yr = yr,wmaster = ward_master)
  dt2 <- dt[,{
    sdev <- sd(get(var))
    mean_val <- mean(get(var))
    sigma <- (get(var) - mean_val)/sdev
    .SD %>% mutate(sigma=sigma)
    }]
  setDT(dt2)
  
  dt2[,expense_class:=case_when(sigma>rsigma ~ "High",sigma<=lsigma ~"Low",T ~ "Normal" )]
  mean_val <-  mean(dt[[var]],na.rm = T)
  mean_label <- mean_val %>% chng_unit(rsunit) %>% paste(prfx,.,rsunit,perlabel) %>% paste("Mean=",.)
  
  low_box_text <-  dt2[expense_class=="Low",ward_name] %>% paste(collapse ="\n")
  high_box_text <-  dt2[expense_class=="High",ward_name] %>% paste(collapse ="\n")
  
  vect <- dt2[[var]]
  stopifnot(length(vect)>0)
  low_thr <- dt2[expense_class=="Low",max(get(var))]
  low_thr_label  <- low_thr %>% chng_unit(rsunit) %>% paste(prfx,.,rsunit,perlabel)
  high_thr <- dt2[expense_class=="High",min(get(var))] 
  high_thr_label  <-  high_thr %>% chng_unit(rsunit) %>% paste(prfx,.,rsunit,perlabel)
  
  g1 <- 
    dt2  %>% 
    ggplot(aes(get(var))) + 
    geom_histogram(aes(fill=expense_class),col="grey36",bins = totbins) +  
    scale_fill_manual(values = c(Low="coral1",Normal="darkgrey",High="lawngreen")) +
    #scale_x_continuous(labels = unit_format(unit = "cr", scale = 1e-7)) +
    scale_x_continuous(labels = function(x) paste(x/10^pwr,ushort)) +
    xlab(dt_axis[colname==var,axisname] %>% paste(rsunit)) + 
    ylab("Wards") +
    geom_vline(xintercept = mean_val,lty=2)
   
labdt <- ggplot_build(plot = g1)$data[[1]] %>% as.data.table()
labdt[,rn:=seq_along(x)]
labdt2 <- labdt[rn<=totbins,.(x,y,xmin,xmax)]
labdt2[,wards:=dt2[get(var)<=xmax & get(var)>=xmin,ward_name %>% unique %>% paste(collapse = "\n") %>% str_replace("\r"," ")],by=x]
labdt2[,status:=case_when(xmax<=low_thr ~ "L",xmin>=high_thr ~ "H", T ~ "N")]

ylim <- labdt$y %>% max # new way to find the scales of a histogram for label positioning
xlim <- labdt$x %>% max


g1  +
  geom_text_repel(data=data.table(x=mean_val,y=max(labdt2$y),label=mean_label),aes(x=x,y=y,label=label),col="blue",size=6,hjust="right") +
  annotate("label",x=xlim,y=ylim,label=Boxtitle,col="blue",size=10,hjust="right",vjust="center") +
  geom_label_repel(data = labdt2[status %in% c("L","H")],mapping = aes(x,y,label=wards),
                   xlim=c(xstart,NA),ylim =c(0,ystart),vjust="top",alpha=0.8,segment.alpha = 0.8,nudge_y = 2) +
  # annotate("label",x =  -Inf,y=Inf,label=low_box_text,size=4,vjust="top",hjust="left",alpha=0.7,fill="coral1") +
  # annotate("label",x = Inf,y=Inf,label=high_box_text,size=4,vjust="top",hjust="right",alpha=0.7,fill="lawngreen") + 
  labs(title = "Ward Histogram",subtitle = paste("Project Types:",Keywords),
       caption = paste0("Wards with very low value (Value < ",chng_unit(low_thr,u = rsunit),rsunit, ") and wards with extremely high value (Value > ", chng_unit(high_thr,rsunit), rsunit,") compared to normal are shown in the callouts")) +
  theme(plot.caption = element_text(size = 10,colour = "red",face = "italic"))

}

# Still to fill up the annotate layers... thinking
create_infog <- function(citzport_data = citzport_test, wardno=150,kpat="Foot",yr=16:20,var1="bud_pcap"){
  dt_axis <- fread("axis_titles.csv")
  Keywords <- citzport_data$keywords %>% str_split(",") %>% unlist %>% str_trim %>% unique %>% str_subset(kpat) %>% paste(collapse =",")
  Years <- yr %>% paste(collapse =",")
  
  dt <- ward_ranks(dt = citzport_data, keypat = kpat,yr = yr,wmaster = ward_master)
  dt <- dt[get(var)<=clip*max(get(var))]
  wname <- dt[ward==wardno,ward_name]
}

waffle_budgets <- function(wno=150,dt=citzport_test,fy=16:20,row=4){
  wname <- ward_names[ward==wno,ward_name]
  waf <- function(dt,y,r) {
    df <- as.data.frame(dt[fy==y])
    wdata <- c(Spent= df[1,2],Unspent=df[1,3])
    waffle(equal = T,wdata,rows = r,colors = c("#228B22","#B0E0E6"))
    }
  dt <- dt[ward==wno,.(bud=sum(approvedamount)/1e7,exp=sum(totbillamt,na.rm = T)/1e7),.(fy)][,unspent:=bud-exp]
  iron(waf(dt,fy,r = row))
}


# ====== PROCESS RECEIPTS ==========

proc_rcpts <- function(pat=".RDS"){
  masterdt <- data.table()
  list.files("~/Dropbox/bbmp",pattern = pat,full.names = T) -> all_files
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


# unpack the DT
proc_dcbills <- function(x1){
  unpack_rtype <- function(str) str %>% map_chr(~read_html(.x) %>% html_text)
  x1[,rtype2:=unpack_rtype(rtype)]
}
