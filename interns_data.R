# intern excel files analysis  - old code no longer needed - just for reference

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

# cluster budget names - now we are clear they are verbatim so no clustering is needed 
get_bud_clust <- function(dt,ht=2){
        dt <- dt[!is.na(budgname)]
        clustbud <- clust_addr(ht,dt,var = "budgname")
        code_lookup <- clustbud[!is.na(clust),.(string,clust,budgcode)][order(clust)] %>% unique
        code_lookup[,poss_codes:=ifelse(is.na(budgcode), unique(budgcode) %>% na.omit %>% paste(collapse= "/"),budgcode),by=clust]
}
